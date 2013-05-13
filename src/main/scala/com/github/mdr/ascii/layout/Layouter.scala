package com.github.mdr.ascii.layout

import com.github.mdr.ascii.layout.drawing._
import com.github.mdr.ascii.layout.layering._
import com.github.mdr.ascii.parser.Dimension
import com.github.mdr.ascii.parser.Point
import com.github.mdr.ascii.parser.Region
import com.github.mdr.ascii.parser.Translatable
import com.github.mdr.ascii.util.Utils

object Layouter {

  private val MINIMUM_VERTEX_HEIGHT = 3

}

class Layouter(vertexRenderingStrategy: VertexRenderingStrategy[_]) {

  import Layouter._

  def layout(layering: Layering): Drawing = {
    val layerInfos: Map[Layer, LayerInfo] = calculateLayerInfos(layering)

    var previousLayerInfo: LayerInfo = LayerInfo(Map())
    var incompleteEdges: Map[DummyVertex, List[Point]] = Map()
    var diagramElements: List[DrawingElement] = Nil
    for (layer ← layering.layers) {
      val RowLayoutResult(elements, updatedLayerInfo, updatedIncompletedEdges) =
        layoutLayer(previousLayerInfo, layerInfos(layer), layering.edges, incompleteEdges)
      previousLayerInfo = updatedLayerInfo
      incompleteEdges = updatedIncompletedEdges
      diagramElements ++= elements
    }
    Drawing(diagramElements)
  }

  /**
   * Calculate layer infos, with vertices given their correct horizontal coordinate, but awaiting
   * a correct vertical coordinate.
   */
  private def calculateLayerInfos(layering: Layering): Map[Layer, LayerInfo] = {
    var layerInfos: Map[Layer, LayerInfo] = Map()
    for ((previousLayerOpt, currentLayer, nextLayerOpt) ← Utils.withPreviousAndNext(layering.layers)) {
      val vertexInfos = calculateLayerInfo(currentLayer, layering.edges, previousLayerOpt, nextLayerOpt)
      layerInfos += currentLayer -> vertexInfos
    }
    spaceVertices(layerInfos)
  }

  /**
   * For each vertex in the layer, calculate its size and assign ports for its in- and out-edges.
   */
  private def calculateLayerInfo(layer: Layer, edges: List[Edge], previousLayerOpt: Option[Layer], nextLayerOpt: Option[Layer]): LayerInfo = {
    val inEdges = previousLayerOpt.map { previousLayer ⇒
      edges.sortBy { case Edge(v1, _) ⇒ previousLayer.vertices.indexOf(v1) }
    }.getOrElse(Nil)
    val outEdges = nextLayerOpt.map { nextLayer ⇒
      edges.sortBy { case Edge(_, v2) ⇒ nextLayer.vertices.indexOf(v2) }
    }.getOrElse(Nil)
    def getInEdges(vertex: Vertex) = inEdges collect { case e @ Edge(v1, `vertex`) ⇒ e }
    def getOutEdges(vertex: Vertex) = outEdges collect { case e @ Edge(`vertex`, v2) ⇒ e }

    def getDimension(vertex: Vertex): Dimension = vertex match {
      case v: RealVertex  ⇒ calculateDimension(v, getInEdges(vertex).size, getOutEdges(vertex).size)
      case _: DummyVertex ⇒ Dimension(height = 1, width = 1)
    }
    val dimensions: Map[Vertex, Dimension] = Utils.makeMap(layer.vertices, getDimension)
    val regions: Map[Vertex, Region] = calculateVertexRegions(layer, dimensions)

    LayerInfo(Utils.makeMap(layer.vertices, v ⇒ makeVertexInfo(v, regions(v), getInEdges(v), getOutEdges(v))))
  }

  private def makeVertexInfo(vertex: Vertex, region: Region, inEdges: List[Edge], outEdges: List[Edge]): VertexInfo =
    vertex match {
      case _: RealVertex ⇒
        val inPorts = portOffsets(inEdges, region.width).mapValues {
          offset ⇒ region.topLeft.right(offset)
        }
        val outPorts = portOffsets(outEdges, region.width).mapValues {
          offset ⇒ region.bottomLeft.right(offset)
        }
        VertexInfo(region, inPorts, outPorts)
      case _: DummyVertex ⇒
        val List(inVertex) = inEdges
        val List(outVertex) = outEdges
        VertexInfo(region, Map(inVertex -> region.topLeft), Map(outVertex -> region.topLeft))
    }

  /**
   * Space out edge ports even along the edge of a vertex.
   */
  private def portOffsets(edges: List[Edge], vertexWidth: Int): Map[Edge, Int] = {
    val factor = vertexWidth / (edges.size + 1)
    val centraliser = (vertexWidth - factor * (edges.size + 1)) / 2
    edges.zipWithIndex.map { case (v, i) ⇒ (v, (i + 1) * factor + centraliser) }.toMap
  }

  /**
   * Calculate dimension based on vertex rendering strategy together with the number of in/out edges
   */
  private def calculateDimension(v: RealVertex, inDegree: Int, outDegree: Int) = {
    val selfEdges = v.selfEdges
    val requiredInputWidth = (inDegree + selfEdges) * 2 + 3
    val requiredOutputWidth = (outDegree + selfEdges) * 2 + 3
    val Dimension(preferredHeight, preferredWidth) = getPreferredSize(vertexRenderingStrategy, v)
    val width = math.max(math.max(requiredInputWidth, requiredOutputWidth), preferredWidth + 2)
    val height = math.max(MINIMUM_VERTEX_HEIGHT, preferredHeight + 2)
    Dimension(height = height, width = width)
  }

  /**
   * Initially pack vertex regions close together (with a single space between them)
   */
  private def calculateVertexRegions(layer: Layer, dimensions: Map[Vertex, Dimension]): Map[Vertex, Region] = {
    var regions: Map[Vertex, Region] = Map()
    var pos = Point(0, 0)
    for (vertex ← layer.vertices) {
      val region = Region(pos, dimensions(vertex))
      regions += vertex -> region
      pos = region.topRight.right(2)
    }
    regions
  }

  private def calculateDiagramWidth(layerInfos: Map[Layer, LayerInfo]) = {
    def vertexWidth(vertexInfo: VertexInfo) = vertexInfo.region.dimension.width
    def layerWidth(layerInfo: LayerInfo) = {
      val vertexInfos = layerInfo.vertexInfos.values
      val spacing = vertexInfos.size
      vertexInfos.map(vertexWidth).sum + spacing
    }
    layerInfos.values.map(layerWidth).fold(0)(_ max _)
  }

  private def spaceVertices(layerInfos: Map[Layer, LayerInfo]): Map[Layer, LayerInfo] = {
    val diagramWidth = calculateDiagramWidth(layerInfos)
    layerInfos.map { case (layer, info) ⇒ layer -> spaceVertices(layer, info, diagramWidth) }
  }

  /**
   * Space out vertices horizontally across the full width of the diagram
   */
  private def spaceVertices(layer: Layer, layerVertexInfos: LayerInfo, diagramWidth: Int): LayerInfo = {
    val excessSpace = diagramWidth - layerVertexInfos.maxColumn
    val horizontalSpacing = math.max(excessSpace / (layerVertexInfos.vertexInfos.size + 1), 1)

    var leftColumn = horizontalSpacing
    val newVertexInfos =
      for {
        v ← layer.vertices
        vertexInfo ← layerVertexInfos.vertexInfo(v)
      } yield {
        val oldLeftColumn = leftColumn
        leftColumn += vertexInfo.region.width
        leftColumn += horizontalSpacing
        v -> vertexInfo.setLeft(oldLeftColumn)
      }
    LayerInfo(newVertexInfos.toMap)
  }

  private def calculateEdgeOrdering(edgeInfos: List[EdgeInfo]): Map[EdgeInfo, Int] = {

    // We sort this way to avoid unnecessary overlaps coming into the same vertex
    val sortedInfos = edgeInfos.sortBy { info ⇒
      val diff = info.startPort.column - info.finishPort.column
      val sign = if (diff == 0) 0 else diff / math.abs(diff)
      sign * info.finishPort.column
    }

    var edgeRows: Map[EdgeInfo, Int] = Map()
    var rowNumber = 0
    for {
      edgeInfo @ EdgeInfo(_, _, startPort, finishPort, _) ← sortedInfos
      if startPort.column != finishPort.column
    } {
      edgeRows += edgeInfo -> rowNumber
      rowNumber += 1
    }

    reorderEdgesWithSameStartAndEndColumns(edgeRows, sortedInfos)
  }

  /**
   * Force edges that share start and end columns to be ordered so as to avoid conflicts
   */
  private def reorderEdgesWithSameStartAndEndColumns(edgeRows: Map[EdgeInfo, Int], sortedInfos: List[EdgeInfo]): Map[EdgeInfo, Int] = {
    var updatedEdgeRows = edgeRows
    var continue = true
    while (continue) {
      continue = false
      for {
        edgeInfo1 @ EdgeInfo(_, _, start1, finish1, _) ← sortedInfos
        edgeInfo2 @ EdgeInfo(_, _, start2, finish2, _) ← sortedInfos
        if edgeInfo1 != edgeInfo2
        if start1.column == finish2.column
        if start2.column != finish1.column // Prevents an infinite loop (issue #3), but still allows overlapping edges
        row1 = updatedEdgeRows(edgeInfo1)
        row2 = updatedEdgeRows(edgeInfo2)
        if row1 > row2
      } {
        updatedEdgeRows += edgeInfo1 -> row2
        updatedEdgeRows += edgeInfo2 -> row1
        continue = true
      }
    }
    updatedEdgeRows
  }

  private case class RowLayoutResult(
    drawingElements: List[DrawingElement],
    layerInfo: LayerInfo,
    updatedIncompletedEdges: Map[DummyVertex, List[Point]])

  private def layoutLayer(
    previousLayerInfo: LayerInfo,
    currentLayerInfo: LayerInfo,
    edges: List[Edge],
    incompleteEdges: Map[DummyVertex, List[Point]]): RowLayoutResult = {

    val edgeInfos: List[EdgeInfo] =
      for {
        edge @ Edge(v1, v2) ← edges
        previousVertexInfo ← previousLayerInfo.vertexInfo(v1)
        currentVertexInfo ← currentLayerInfo.vertexInfo(v2)
        start = previousVertexInfo.outPorts(edge).down
        finish = currentVertexInfo.inPorts(edge).up
      } yield EdgeInfo(v1, v2, start, finish, edge.reversed)

    val edgeRows: Map[EdgeInfo, Int] = calculateEdgeOrdering(edgeInfos)

    val edgeZoneTopRow = if (previousLayerInfo.isEmpty) -1 /* first layer */ else previousLayerInfo.maxRow + 1
    def edgeBendRow(rowIndex: Int) = edgeZoneTopRow + rowIndex * 1 /* 2 */ + 1

    val edgeZoneBottomRow =
      if (edgeInfos.isEmpty)
        -1
      else if (edgeRows.isEmpty)
        edgeZoneTopRow + 2
      else
        edgeBendRow(edgeRows.values.max) + 2

    val edgeInfoToPoints: Map[EdgeInfo, List[Point]] =
      (for (edgeInfo @ EdgeInfo(startVertex, _, start, finish, _) ← edgeInfos) yield {
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
      for ((EdgeInfo(_, finishVertex: RealVertex, _, _, reversed), points) ← edgeInfoToPoints)
        yield EdgeDrawingElement(points, reversed, !reversed)

    val updatedIncompleteEdges: Map[DummyVertex, List[Point]] =
      for ((EdgeInfo(_, finishVertex: DummyVertex, _, _, _), points) ← edgeInfoToPoints)
        yield finishVertex -> points.init

    val updatedLayerInfo = currentLayerInfo.down(edgeZoneBottomRow + 1)

    val vertexElements = updatedLayerInfo.realVertexInfos.map {
      case (realVertex, info) ⇒
        val text = getText(vertexRenderingStrategy, realVertex, info.contentRegion.dimension)
        VertexDrawingElement(info.region, text)
    }
    RowLayoutResult(vertexElements ++ edgeElements, updatedLayerInfo, updatedIncompleteEdges)
  }

  private def getPreferredSize[V](vertexRenderingStrategy: VertexRenderingStrategy[V], realVertex: RealVertex) =
    vertexRenderingStrategy.getPreferredSize(realVertex.contents.asInstanceOf[V])

  private def getText[V](vertexRenderingStrategy: VertexRenderingStrategy[V], realVertex: RealVertex, preferredSize: Dimension) =
    vertexRenderingStrategy.getText(realVertex.contents.asInstanceOf[V], preferredSize)

}

/**
 * Info for rendering a vertex.
 *
 * @param region - area allocated to the vertex.
 * @param inPorts - map of incoming edges to the points they connect to on the vertex box.
 * @param outPorts - map of outgoing edges to the points they connect to on the vertex box.
 */
case class VertexInfo(region: Region, inPorts: Map[Edge, Point], outPorts: Map[Edge, Point]) extends Translatable[VertexInfo] {

  def contentRegion: Region = region.copy(topLeft = region.topLeft.down.right, bottomRight = region.bottomRight.up.left)

  def translate(down: Int = 0, right: Int = 0): VertexInfo = {
    val newRegion = region.translate(down, right)
    VertexInfo(
      region.translate(down, right),
      Utils.transformValues(inPorts)(_.translate(down, right)),
      Utils.transformValues(outPorts)(_.translate(down, right)))
  }

  def setLeft(column: Int): VertexInfo = translate(right = column - region.leftColumn)

}

case class EdgeInfo(startVertex: Vertex, finishVertex: Vertex, startPort: Point, finishPort: Point, reversed: Boolean)

/**
 * VertexInfo's for all the vertices in a single layer.
 */
case class LayerInfo(vertexInfos: Map[Vertex, VertexInfo]) extends Translatable[LayerInfo] {

  def vertexInfo(v: Vertex): Option[VertexInfo] = vertexInfos.get(v)

  def isEmpty = vertexInfos.isEmpty

  def maxRow = vertexInfos.values.map(_.region.bottomRow).fold(0)(_ max _)

  def maxColumn = vertexInfos.values.map(_.region.rightColumn).fold(0)(_ max _)

  def translate(down: Int = 0, right: Int = 0) =
    copy(vertexInfos = Utils.transformValues(vertexInfos)(_.translate(down, right)))

  def realVertexInfos: List[(RealVertex, VertexInfo)] = vertexInfos.toList.collect {
    case (vertex: RealVertex, info) ⇒ (vertex, info)
  }

}

