package com.github.mdr.ascii.layout

import com.github.mdr.ascii._
import com.github.mdr.ascii.util.Utils

class Layouter[V](vertexRenderingStrategy: VertexRenderingStrategy[V]) {

  private case class VertexInfo(region: Region, inPorts: Map[Edge, Point], outPorts: Map[Edge, Point]) {

    def contentRegion: Region = region.copy(topLeft = region.topLeft.down.right, bottomRight = region.bottomRight.up.left)

    def translate(down: Int = 0, right: Int = 0): VertexInfo = {
      val newRegion = region.translate(down, right)
      VertexInfo(
        region.translate(down, right),
        Utils.transformValues(inPorts)(_.translate(down, right)),
        Utils.transformValues(outPorts)(_.translate(down, right)))
    }

  }

  val VERTEX_HEIGHT = 3

  private def calculateVertexInfo(layer: Layer, edges: List[Edge], previousLayer: Layer, nextLayer: Layer): Map[Vertex, VertexInfo] = {
    val inEdges = edges.sortBy { case Edge(v1, _) ⇒ previousLayer.vertices.indexOf(v1) }
    val outEdges = edges.sortBy { case Edge(_, v2) ⇒ nextLayer.vertices.indexOf(v2) }
    def inVertices(vertex: Vertex) = inEdges collect { case e @ Edge(v1, `vertex`) ⇒ e }
    def outVertices(vertex: Vertex) = outEdges collect { case e @ Edge(`vertex`, v2) ⇒ e }

    val dimensions: Map[Vertex, Dimension] =
      (for {
        vertex ← layer.vertices
        outDegree = outVertices(vertex).size
        inDegree = inVertices(vertex).size
      } yield {
        val dimension = vertex match {
          case realVertex: RealVertex ⇒
            val Dimension(preferredHeight, preferredWidth) =
              vertexRenderingStrategy.getPreferredSize(realVertex.contents.asInstanceOf[V])
            val width = math.max(math.max(outDegree * 2 + 3, inDegree * 2 + 3), preferredWidth + 2)
            val height = math.max(VERTEX_HEIGHT, preferredHeight + 2)
            Dimension(height = height, width = width)
          case _: DummyVertex ⇒
            Dimension(height = 1, width = 1)
        }
        vertex -> dimension
      }).toMap

    var regions: Map[Vertex, Region] = Map()
    var pos = Point(0, 0)
    for (vertex ← layer.vertices) {
      val region = Region(pos, dimensions(vertex))
      regions += vertex -> region
      pos = region.topRight.right(2)
    }

    def makeVertexInfo(vertex: Vertex): VertexInfo = {
      val region = regions(vertex)
      vertex match {
        case _: RealVertex ⇒
          val inPorts = (for ((vertex, index) ← inVertices(vertex).zipWithIndex)
            yield vertex -> region.topLeft.right(index * 2 + 2)).toMap
          val outPorts = (for ((vertex, index) ← outVertices(vertex).zipWithIndex)
            yield vertex -> region.bottomLeft.right(index * 2 + 2)).toMap
          VertexInfo(region, inPorts, outPorts)
        case _: DummyVertex ⇒
          val List(inVertex) = inVertices(vertex)
          val List(outVertex) = outVertices(vertex)
          VertexInfo(region, Map(inVertex -> region.topLeft), Map(outVertex -> region.topLeft))
      }
    }

    (for (vertex ← layer.vertices) yield vertex -> makeVertexInfo(vertex)).toMap
  }

  case class EdgeInfo(startVertex: Vertex, finishVertex: Vertex, startPort: Point, finishPort: Point)

  private def calculateEdgeOrdering(edgeInfos: List[EdgeInfo]): Map[EdgeInfo, Int] = {

    // We sort this way to avoid unnecessary overlaps coming into the same vertex
    val sortedInfos = edgeInfos.sortBy { info ⇒
      val diff = info.startPort.column - info.finishPort.column
      val sign = if (diff == 0) 0 else diff / math.abs(diff)
      sign * info.finishPort.column
    }

    var edgeRows: Map[EdgeInfo, Int] = Map()
    var rowNumber = 0
    for { edgeInfo @ EdgeInfo(_, _, startPort, finishPort) ← sortedInfos } {
      if (startPort.column != finishPort.column) {
        edgeRows += edgeInfo -> rowNumber
        rowNumber += 1
      }
    }

    // Force edges that share start and end columns to be ordered correctly to avoid conflicts
    var continue = true
    while (continue) {
      continue = false
      for {
        edgeInfo1 @ EdgeInfo(_, _, start1, _) ← sortedInfos
        edgeInfo2 @ EdgeInfo(_, _, _, finish2) ← sortedInfos
        if edgeInfo1 != edgeInfo2
        if start1.column == finish2.column
        row1 = edgeRows(edgeInfo1)
        row2 = edgeRows(edgeInfo2)
        if row1 > row2
      } {
        edgeRows += edgeInfo1 -> row2
        edgeRows += edgeInfo2 -> row1
        continue = true
      }
    }

    edgeRows
  }

  private def layoutRow(vertexInfos1: Map[Vertex, VertexInfo], vertexInfos2: Map[Vertex, VertexInfo], edges: List[Edge], incompleteEdges: Map[DummyVertex, List[Point]]): (List[DrawingElement], Map[Vertex, VertexInfo], Map[DummyVertex, List[Point]]) = {

    val edgeInfos =
      for {
        edge @ Edge(v1, v2) ← edges
        vertexInfo1 ← vertexInfos1.get(v1)
        vertexInfo2 ← vertexInfos2.get(v2)
        start = vertexInfo1.outPorts(edge).down
        finish = vertexInfo2.inPorts(edge).up
      } yield EdgeInfo(v1, v2, start, finish)

    val edgeRows = calculateEdgeOrdering(edgeInfos)

    val edgeZoneTopRow = if (vertexInfos1.isEmpty) -1 /* first layer */ else vertexInfos1.values.map(_.region.bottomRow).max + 1
    def edgeBendRow(rowIndex: Int) = edgeZoneTopRow + rowIndex * 2 + 1

    val edgeZoneBottomRow =
      if (edgeInfos.isEmpty)
        -1
      else if (edgeRows.isEmpty)
        edgeZoneTopRow + 2
      else
        edgeBendRow(edgeRows.values.max) + 2

    val edgeInfoToPoints: Map[EdgeInfo, List[Point]] =
      (for (edgeInfo @ EdgeInfo(startVertex, _, start, finish) ← edgeInfos) yield {
        val trueFinish = finish.translate(down = edgeZoneBottomRow + 1)
        val priorPoints: List[Point] = startVertex match {
          case dv: DummyVertex ⇒ incompleteEdges(dv)
          case _: RealVertex   ⇒ List(start)
        }
        val lastPriorPoint = priorPoints.last
        val points =
          if (lastPriorPoint.column == trueFinish.column) // No bend required
            priorPoints :+ trueFinish
          else {
            val row = edgeBendRow(edgeRows(edgeInfo))
            priorPoints ++ List(lastPriorPoint.copy(row = row), trueFinish.copy(row = row), trueFinish)
          }
        edgeInfo -> points
      }).toMap

    val edgeElements =
      for ((EdgeInfo(_, finishVertex: RealVertex, _, _), points) ← edgeInfoToPoints)
        yield EdgeDrawingElement(points, false, true)

    val updatedIncompleteEdges: Map[DummyVertex, List[Point]] =
      for ((EdgeInfo(_, finishVertex: DummyVertex, _, _), points) ← edgeInfoToPoints)
        yield finishVertex -> points.init

    val updatedVertexInfos2 = Utils.transformValues(vertexInfos2)(_.translate(down = edgeZoneBottomRow + 1))

    val vertexElements = updatedVertexInfos2.toList.collect {
      case (vertex: RealVertex, info) ⇒
        val text = vertexRenderingStrategy.getText(vertex.contents.asInstanceOf[V], info.contentRegion.dimension)
        VertexDrawingElement(info.region, text)
    }
    (vertexElements ++ edgeElements, updatedVertexInfos2, updatedIncompleteEdges)
  }

  def layout(layering: Layering): List[DrawingElement] = {

    var previousVertexInfos: Map[Vertex, VertexInfo] = Map()
    var incompleteEdges: Map[DummyVertex, List[Point]] = Map()
    (for ((previousVerticesOpt, currentVertices, nextVerticesOpt) ← Utils.withPreviousAndNext(layering.layers)) yield {
      val previousVertices = previousVerticesOpt.getOrElse(Layer(Nil))
      val nextVertices = nextVerticesOpt.getOrElse(Layer(Nil))
      val vertexInfos = calculateVertexInfo(currentVertices, layering.edges, previousVertices, nextVertices)
      val (elements, updatedVertexInfos, updatedIncompletedEdges) =
        layoutRow(previousVertexInfos, vertexInfos, layering.edges, incompleteEdges)
      previousVertexInfos = updatedVertexInfos
      incompleteEdges = updatedIncompletedEdges
      elements
    }).flatten

  }

}