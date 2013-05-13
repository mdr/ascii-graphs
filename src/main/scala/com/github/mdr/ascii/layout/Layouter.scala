package com.github.mdr.ascii.layout

import com.github.mdr.ascii.layout.drawing._
import com.github.mdr.ascii.layout.layering._
import com.github.mdr.ascii.parser.Dimension
import com.github.mdr.ascii.parser.Point
import com.github.mdr.ascii.parser.Region
import com.github.mdr.ascii.parser.Translatable
import com.github.mdr.ascii.util.Utils
import com.github.mdr.ascii.util.Utils._

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
      val LayerLayoutResult(elements, updatedLayerInfo, updatedIncompletedEdges) =
        layoutLayer(previousLayerInfo, layerInfos(layer), layering.edges, incompleteEdges)
      previousLayerInfo = updatedLayerInfo
      incompleteEdges = updatedIncompletedEdges
      diagramElements ++= elements
    }
    Drawing(diagramElements)
  }

  /**
   * Calculate layer infos, with vertices given their correct column coordinates, but awaiting
   * correct row coordinates.
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
    val dimensions: Map[Vertex, Dimension] = makeMap(layer.vertices, getDimension)
    val regions: Map[Vertex, (Region, Region)] = calculateVertexRegions(layer, dimensions)

    LayerInfo(makeMap(layer.vertices, v ⇒ makeVertexInfo(v, regions(v)._1, regions(v)._2, getInEdges(v), getOutEdges(v))))
  }

  private def makeVertexInfo(vertex: Vertex, boxRegion: Region, greaterRegion: Region, inEdges: List[Edge], outEdges: List[Edge]): VertexInfo =
    vertex match {
      case realVertex: RealVertex ⇒
        val inPorts = portOffsets(inEdges, boxRegion.width, realVertex.selfEdges).mapValues {
          offset ⇒ boxRegion.topLeft.right(offset)
        }
        val outPorts = portOffsets(outEdges, boxRegion.width, realVertex.selfEdges).mapValues {
          offset ⇒ boxRegion.bottomLeft.right(offset)
        }
        VertexInfo(boxRegion, inPorts, outPorts)
      case _: DummyVertex ⇒
        val List(inVertex) = inEdges
        val List(outVertex) = outEdges
        VertexInfo(boxRegion, Map(inVertex -> boxRegion.topLeft), Map(outVertex -> boxRegion.topLeft))
    }

  /**
   * Space out edge ports even along the edge of a vertex.
   *
   * We leave room for self edges at the right
   */
  private def portOffsets(edges: List[Edge], vertexWidth: Int, selfEdges: Int): Map[Edge, Int] = {
    val factor = vertexWidth / (edges.size + selfEdges + 1)
    val centraliser = (vertexWidth - factor * (edges.size + selfEdges + 1)) / 2
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
   * Initially pack vertex regions close together, so we can determine the minimum width of the entire
   * drawing.
   *
   * @return pair of regions: first = region of the vertex box; second = "great region" which includes any
   *   self edges which need to wrap around the vertex.
   */
  private def calculateVertexRegions(layer: Layer, dimensions: Map[Vertex, Dimension]): Map[Vertex, (Region, Region)] = {
    var regions: Map[Vertex, (Region, Region)] = Map()
    var pos = Point(0, 0)
    for (vertex ← layer.vertices) {
      val boxRegion = Region(pos, dimensions(vertex))
      val selfEdgesSpacing = vertex match {
        case realVertex: RealVertex if realVertex.selfEdges > 0 ⇒ 1 + realVertex.selfEdges
        case _                                                  ⇒ 0
      }
      val greaterRegion = boxRegion.expandRight(selfEdgesSpacing)
      regions += vertex -> (boxRegion, greaterRegion)
      pos = greaterRegion.topRight.right(2)
    }
    regions
  }

  private def calculateDiagramWidth(layerInfos: Map[Layer, LayerInfo]) = {
    def vertexWidth(vertexInfo: VertexInfo) = vertexInfo.boxRegion.dimension.width
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
        leftColumn += vertexInfo.boxRegion.width
        leftColumn += horizontalSpacing
        v -> vertexInfo.setLeft(oldLeftColumn)
      }
    LayerInfo(newVertexInfos.toMap)
  }

  private case class LayerLayoutResult(
    drawingElements: List[DrawingElement],
    layerInfo: LayerInfo,
    updatedIncompletedEdges: Map[DummyVertex, List[Point]])

  /**
   * 1) Decide the (vertical) order of edges coming into the currentLayer -- that is, what row they bend on (if required)
   * 2) Decide the vertical position of the vertices in the currentLayer.
   * 3) Render the incoming edges and current layer vertices into diagram elements.
   * 4) Update bookkeeping information about as-yet-incomplete edges.
   *
   * @param edges -- edges from the previous layer into the current layer
   * @param incompleteEdges -- map from a dummy vertex in previous layer (which is the bottom-most tip of an incomplete
   *                           long edge), to the sequence of points that make up the edge built so far.
   */
  private def layoutLayer(
    previousLayerInfo: LayerInfo,
    currentLayerInfo: LayerInfo,
    edges: List[Edge],
    incompleteEdges: Map[DummyVertex, List[Point]]): LayerLayoutResult = {

    val edgeInfos: List[EdgeInfo] = makeEdgeInfos(edges, previousLayerInfo, currentLayerInfo)

    val edgeZoneTopRow = if (previousLayerInfo.isEmpty) -1 /* first layer */ else previousLayerInfo.maxRow + 1
    val edgeBendCalculator = new EdgeBendCalculator(edgeInfos, edgeZoneTopRow)

    val edgeInfoToPoints: Map[EdgeInfo, List[Point]] =
      makeMap(edgeInfos, edgeInfo ⇒ getEdgePoints(edgeInfo, edgeBendCalculator, incompleteEdges))

    val updatedIncompleteEdges: Map[DummyVertex, List[Point]] =
      for ((EdgeInfo(_, finishVertex: DummyVertex, _, _, _), points) ← edgeInfoToPoints)
        yield finishVertex -> points.init

    val updatedLayerInfo = currentLayerInfo.down(edgeBendCalculator.edgeZoneBottomRow + 1)

    val vertexElements = makeVertexElements(updatedLayerInfo)
    val edgeElements = makeEdgeElements(edgeInfoToPoints)
    LayerLayoutResult(vertexElements ++ edgeElements, updatedLayerInfo, updatedIncompleteEdges)
  }

  private def makeEdgeInfos(edges: List[Edge], previousLayerInfo: LayerInfo, currentLayerInfo: LayerInfo): List[EdgeInfo] =
    for {
      edge @ Edge(v1, v2) ← edges
      previousVertexInfo ← previousLayerInfo.vertexInfo(v1)
      currentVertexInfo ← currentLayerInfo.vertexInfo(v2)
      start = previousVertexInfo.outPorts(edge).down
      finish = currentVertexInfo.inPorts(edge).up // Note that this will be at the wrong absolute vertical position, we'll adjust later
    } yield EdgeInfo(v1, v2, start, finish, edge.reversed)

  private def getEdgePoints(edgeInfo: EdgeInfo, edgeBendCalculator: EdgeBendCalculator, incompleteEdges: Map[DummyVertex, List[Point]]): List[Point] = {
    val EdgeInfo(startVertex, _, start, finish, _) = edgeInfo
    val trueFinish = finish.translate(down = edgeBendCalculator.edgeZoneBottomRow + 1)
    val priorPoints: List[Point] = startVertex match {
      case dv: DummyVertex ⇒ incompleteEdges(dv)
      case _: RealVertex   ⇒ List(start)
    }
    val lastPriorPoint = priorPoints.last
    if (lastPriorPoint.column == trueFinish.column) // No bend required
      priorPoints :+ trueFinish
    else {
      val row = edgeBendCalculator.bendRow(edgeInfo)
      priorPoints ++ List(lastPriorPoint.copy(row = row), trueFinish.copy(row = row), trueFinish)
    }
  }

  private def makeEdgeElements(edgeInfoToPoints: Map[EdgeInfo, List[Point]]): List[EdgeDrawingElement] =
    for ((EdgeInfo(_, finishVertex: RealVertex, _, _, reversed), points) ← edgeInfoToPoints.toList)
      yield EdgeDrawingElement(points, reversed, !reversed)

  private def makeVertexElements(layerInfo: LayerInfo): List[VertexDrawingElement] =
    layerInfo.realVertexInfos.map {
      case (realVertex, info) ⇒
        val text = getText(vertexRenderingStrategy, realVertex, info.contentRegion.dimension)
        VertexDrawingElement(info.boxRegion, text)
    }

  private def getPreferredSize[V](vertexRenderingStrategy: VertexRenderingStrategy[V], realVertex: RealVertex) =
    vertexRenderingStrategy.getPreferredSize(realVertex.contents.asInstanceOf[V])

  private def getText[V](vertexRenderingStrategy: VertexRenderingStrategy[V], realVertex: RealVertex, preferredSize: Dimension) =
    vertexRenderingStrategy.getText(realVertex.contents.asInstanceOf[V], preferredSize)

}

