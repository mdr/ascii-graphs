package com.github.mdr.ascii.layout

import com.github.mdr.ascii._
import com.github.mdr.ascii.util.Utils

sealed abstract class Vertex
class DummyVertex() extends Vertex
class RealVertex(val text: String) extends Vertex { override def toString = "RealVertex(" + text + ")" }

object Layouter {

  private case class VertexInfo(region: Region, inPorts: Map[Edge, Point], outPorts: Map[Edge, Point]) {

    def translate(down: Int = 0, right: Int = 0): VertexInfo = {
      val newRegion = region.translate(down, right)
      VertexInfo(
        region.translate(down, right),
        Utils.transformValues(inPorts)(_.translate(down, right)),
        Utils.transformValues(outPorts)(_.translate(down, right)))
    }

  }

  private class Edge(val startVertex: Vertex, val finishVertex: Vertex)
  private object Edge {
    def unapply(e: Edge) = Some((e.startVertex, e.finishVertex))
  }

  val VERTEX_HEIGHT = 3

  private def calculateVertexInfo(vertices: List[Vertex], inEdges: List[Edge], outEdges: List[Edge]): Map[Vertex, VertexInfo] = {
    def inVertices(vertex: Vertex) = inEdges collect { case e @ Edge(v1, `vertex`) ⇒ e }
    def outVertices(vertex: Vertex) = outEdges collect { case e @ Edge(`vertex`, v2) ⇒ e }
    val dimensions: Map[Vertex, Dimension] =
      (for {
        vertex ← vertices
        outDegree = outVertices(vertex).size
        inDegree = inVertices(vertex).size
        width = math.max(outDegree * 2 + 3, inDegree * 2 + 3)
        dimension = Dimension(height = VERTEX_HEIGHT, width = width)
      } yield vertex -> dimension).toMap

    var regions: Map[Vertex, Region] = Map()
    var pos = Point(0, 0)
    for (vertex ← vertices) {
      val region = Region(pos, dimensions(vertex))
      regions += vertex -> region
      pos = region.topRight.right(2)
    }

    def makeVertexInfo(vertex: Vertex): VertexInfo = {
      val region = regions(vertex)
      val point = region.bottomLeft

      val inPorts = (for ((vertex, index) ← inVertices(vertex).zipWithIndex)
        yield vertex -> region.topLeft.right(index * 2 + 2)).toMap
      val outPorts = (for ((vertex, index) ← outVertices(vertex).zipWithIndex)
        yield vertex -> region.bottomLeft.right(index * 2 + 2)).toMap
      VertexInfo(region, inPorts, outPorts)
    }

    (for (vertex ← vertices) yield vertex -> makeVertexInfo(vertex)).toMap
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
    var continue = false // true
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

  private def layoutRow(vertexInfos1: Map[Vertex, VertexInfo], vertexInfos2: Map[Vertex, VertexInfo], edges: List[Edge]): (List[DrawingElement], Map[Vertex, VertexInfo]) = {

    val edgeInfos =
      for {
        edge @ Edge(v1, v2) ← edges
        vertexInfo1 ← vertexInfos1.get(v1)
        vertexInfo2 ← vertexInfos2.get(v2)
        start = vertexInfo1.outPorts(edge).down
        finish = vertexInfo2.inPorts(edge).up
      } yield EdgeInfo(v1, v2, start, finish)

    val edgeRows = calculateEdgeOrdering(edgeInfos)

    val initRow = if (vertexInfos1.isEmpty) 0 else vertexInfos1.values.map(_.region.bottomRow).head
    def rowCoord(rowIndex: Int) = initRow + rowIndex * 2 + 2

    val edgeFinishRow = (if (edgeRows.isEmpty) 0 else rowCoord(edgeRows.values.max) + 3)

    val edgeElements =
      for (edgeInfo @ EdgeInfo(_, _, start, finish) ← edgeInfos) yield {
        val trueFinish = finish.translate(down = edgeFinishRow)
        val points =
          if (start.column == trueFinish.column)
            List(start, trueFinish) // No bend required
          else {
            val row = rowCoord(edgeRows(edgeInfo))
            List(start, start.copy(row = row), trueFinish.copy(row = row), trueFinish)
          }
        EdgeDrawingElement(points.distinct, false, true)
      }

    val newVertexInfos2 = Utils.transformValues(vertexInfos2)(_.translate(down = edgeFinishRow))

    val elements =
      newVertexInfos2.toList.collect {
        case (vertex: RealVertex, info) ⇒ VertexDrawingElement(info.region, List(vertex.text))
      } ++ edgeElements
    (elements, newVertexInfos2)
  }

  def layout(vertices1: List[Vertex], vertices2: List[Vertex], vertices3: List[Vertex], edgePairs: List[(Vertex, Vertex)]): List[DrawingElement] = {

    val edges = edgePairs.map { case (v1, v2) ⇒ new Edge(v1, v2) }

    val vertexInfos1 = calculateVertexInfo(vertices1, Nil, edges.sortBy { case Edge(_, v2) ⇒ vertices2.indexOf(v2) })
    val vertexInfos2 = calculateVertexInfo(vertices2, edges.sortBy { case Edge(v1, _) ⇒ vertices1.indexOf(v1) }, edges.sortBy { case Edge(_, v2) ⇒ vertices3.indexOf(v2) }) //5 + edges.size * 2
    val vertexInfos3 = calculateVertexInfo(vertices3, edges.sortBy { case Edge(v1, _) ⇒ vertices3.indexOf(v1) }, Nil)

    val (elements1, newVertexInfo1s) = layoutRow(Map(), vertexInfos1, edges)
    val (elements2, newVertexInfo2s) = layoutRow(newVertexInfo1s, vertexInfos2, edges)
    val (elements3, newVertexInfo3s) = layoutRow(newVertexInfo2s, vertexInfos3, edges)
    elements1 ++ elements2 ++ elements3
  }

}