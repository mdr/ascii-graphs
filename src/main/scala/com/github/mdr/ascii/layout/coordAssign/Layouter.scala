package com.github.mdr.ascii.layout.coordAssign

import com.github.mdr.ascii.layout.drawing._
import com.github.mdr.ascii.layout.layering._
import com.github.mdr.ascii.common._
import com.github.mdr.ascii.util.Utils
import com.github.mdr.ascii.util.Utils._

object Layouter {

  private val MINIMUM_VERTEX_HEIGHT = 3

}

class Layouter(vertexRenderingStrategy: VertexRenderingStrategy[_], vertical: Boolean = true) {

  import Layouter._

  case class LayoutState(previousLayerInfo: LayerInfo, incompleteEdges: Map[DummyVertex, List[Point]], drawingElements: List[DrawingElement]) {

    def mergeLayerResult(result: LayerLayoutResult): LayoutState = {
      val LayerLayoutResult(elements, updatedLayerInfo, updatedIncompletedEdges) = result
      LayoutState(updatedLayerInfo, updatedIncompletedEdges, drawingElements ++ elements)
    }

  }

  def layout(layering: Layering): Drawing = {
    val layerInfos: Map[Layer, LayerInfo] = calculateLayerInfos(layering)

    var layoutState = LayoutState(LayerInfo(Map()), Map(), Nil)
    for (layer ← layering.layers) {
      val layerResult = layoutLayer(layoutState.previousLayerInfo, layerInfos(layer), layering.edges, layoutState.incompleteEdges)
      layoutState = layoutState.mergeLayerResult(layerResult)
    }

    Drawing(layoutState.drawingElements)
  }

  /**
   * Calculate layer infos, with vertices given their correct column coordinates, but awaiting
   * correct row coordinates.
   */
  private def calculateLayerInfos(layering: Layering): Map[Layer, LayerInfo] = {
    var layerInfos: Map[Layer, LayerInfo] = Map()
    for ((previousLayerOpt, currentLayer, nextLayerOpt) ← Utils.withPreviousAndNext(layering.layers)) {
      val layerInfo = calculateLayerInfo(currentLayer, layering.edges, previousLayerOpt, nextLayerOpt)
      layerInfos += currentLayer -> layerInfo
    }
    PortNudger.nudge(layering, spaceVertices(layerInfos))
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
      case v: RealVertex  ⇒ calculateVertexDimension(v, getInEdges(vertex).size, getOutEdges(vertex).size)
      case _: DummyVertex ⇒ Dimension(height = 1, width = 1)
    }
    val dimensions: Map[Vertex, Dimension] = makeMap(layer.vertices, getDimension)
    val regions: Map[Vertex, (Region, Region)] = calculateVertexRegions(layer, dimensions)
    def buildVertexInfo(v: Vertex) = {
      val (boxRegion, greaterRegion) = regions(v)
      makeVertexInfo(v, boxRegion, greaterRegion, getInEdges(v), getOutEdges(v))
    }
    LayerInfo(makeMap(layer.vertices, buildVertexInfo))
  }

  private def makeVertexInfo(vertex: Vertex, boxRegion: Region, greaterRegion: Region, inEdges: List[Edge], outEdges: List[Edge]): VertexInfo =
    vertex match {
      case realVertex: RealVertex   ⇒ makeVertexInfo(realVertex, boxRegion, greaterRegion, inEdges, outEdges)
      case dummyVertex: DummyVertex ⇒ makeVertexInfo(dummyVertex, boxRegion, greaterRegion, inEdges, outEdges)
    }

  private def makeVertexInfo(vertex: RealVertex, boxRegion: Region, greaterRegion: Region, inEdges: List[Edge], outEdges: List[Edge]): VertexInfo = {
    val inDegree = inEdges.size + vertex.selfEdges
    val inPorts: List[Point] = portOffsets(inDegree, boxRegion.width).map(boxRegion.topLeft.right)
    val inEdgeToPortMap = inEdges.zip(inPorts).toMap
    val selfInPorts = inPorts.drop(inEdges.size)

    val outDegree = outEdges.size + vertex.selfEdges
    val outPorts = portOffsets(outDegree, boxRegion.width).map(boxRegion.bottomLeft.right)
    val outEdgeToPortMap = outEdges.zip(outPorts).toMap
    val selfOutPorts = outPorts.drop(outEdges.size)

    VertexInfo(boxRegion, greaterRegion, inEdgeToPortMap, outEdgeToPortMap, selfInPorts, selfOutPorts)
  }

  private def makeVertexInfo(vertex: DummyVertex, boxRegion: Region, greaterRegion: Region, inEdges: List[Edge], outEdges: List[Edge]): VertexInfo = {
    val (List(inVertex), List(outVertex)) = (inEdges, outEdges)
    val port = boxRegion.topLeft
    val inEdgeToPortMap = Map(inVertex -> port)
    val outEdgeToPortMap = Map(outVertex -> port)
    VertexInfo(boxRegion, greaterRegion, inEdgeToPortMap, outEdgeToPortMap, Nil, Nil)
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
   * Space out edge ports evenly along the top or bottom edge of a vertex.
   */
  private def portOffsets(portCount: Int, vertexWidth: Int): List[Int] = {
    val factor = vertexWidth / (portCount + 1)
    val centraliser = (vertexWidth - factor * (portCount + 1)) / 2
    0.until(portCount).toList.map(i ⇒ (i + 1) * factor + centraliser)
  }

  /**
   * Calculate dimension based on vertex rendering strategy together with the number of in/out edges
   */
  private def calculateVertexDimension(v: RealVertex, inDegree: Int, outDegree: Int) = {
    val selfEdges = v.selfEdges
    def requiredWidth(degree: Int) =
      if (vertical)
        (degree + selfEdges) * 2 + 1 + 2
      else {
        // We could draw horizontal diagrams more compactly , but the PortNudger requires 
        // space at the moment.
        // degree + selfEdges + 2  
        (degree + selfEdges) * 2 + 1 + 2
      }
    val requiredInputWidth = requiredWidth(inDegree)
    val requiredOutputWidth = requiredWidth(outDegree)
    val Dimension(preferredHeight, preferredWidth) = getPreferredSize(vertexRenderingStrategy, v)
    val width = math.max(math.max(requiredInputWidth, requiredOutputWidth), preferredWidth + 2)
    val height = math.max(MINIMUM_VERTEX_HEIGHT, preferredHeight + 2)
    Dimension(height = height, width = width)
  }

  /**
   * Initially pack vertex regions close together, so we can determine the minimum width of the entire
   * drawing.
   *
   * @return pair of regions: first = region of the vertex box; second = "great region" which includes the space for any
   *   self edges which need to wrap around the vertex.
   */
  private def calculateVertexRegions(layer: Layer, dimensions: Map[Vertex, Dimension]): Map[Vertex, (Region, Region)] = {
    var regions: Map[Vertex, (Region, Region)] = Map()
    var nextVertexTopLeft = Point(0, 0)
    for (vertex ← layer.vertices) {
      val boxRegion = Region(nextVertexTopLeft, dimensions(vertex))
      // Spacing to the right of the vertex for self edges to wrap aroud:
      val selfEdgesSpacing = vertex match {
        case realVertex: RealVertex if realVertex.selfEdges > 0 ⇒ realVertex.selfEdges * 2
        case _                                                  ⇒ 0
      }
      val greaterRegion = boxRegion.expandRight(selfEdgesSpacing).expandUp(selfEdgesSpacing).expandDown(selfEdgesSpacing)
      regions += vertex -> (boxRegion, greaterRegion)
      nextVertexTopLeft = boxRegion.topRight.right(selfEdgesSpacing + 2)
    }
    regions
  }

  /**
   * Calculate the width of the diagram (assuming the widest row is packed together as closely as possible)
   */
  private def calculateDiagramWidth(layerInfos: Map[Layer, LayerInfo]) = {
    def vertexWidth(vertexInfo: VertexInfo) = vertexInfo.greaterRegion.width
    def layerWidth(layerInfo: LayerInfo) = {
      val vertexInfos = layerInfo.vertexInfos.values
      val spacing = vertexInfos.size
      vertexInfos.map(vertexWidth).sum + spacing - 1
    }
    layerInfos.values.map(layerWidth).fold(0)(_ max _)
  }

  private def spaceVertices(layerInfos: Map[Layer, LayerInfo]): Map[Layer, LayerInfo] = {
    val diagramWidth = calculateDiagramWidth(layerInfos)
    layerInfos.map { case (layer, info) ⇒ layer -> spaceVertices(layer, info, diagramWidth) }
  }

  /**
   * Space out vertices horizontally across the full width of the diagram, and centre them vertically within
   * the layer.
   */
  private def spaceVertices(layer: Layer, layerVertexInfos: LayerInfo, diagramWidth: Int): LayerInfo = {
    val excessSpace = diagramWidth - layerVertexInfos.maxColumn
    val horizontalSpacing = math.max(excessSpace / (layerVertexInfos.vertexInfos.size + 1), 1)

    // Height of the vertices in the layer (excluding self edges)
    val layerHeight = layerVertexInfos.vertexInfos.values.map(_.boxRegion.height).max

    var leftColumn = horizontalSpacing
    val newVertexInfos =
      for {
        v ← layer.vertices
        vertexInfo ← layerVertexInfos.vertexInfo(v)
      } yield {
        val oldLeftColumn = leftColumn
        leftColumn += vertexInfo.greaterRegion.width
        leftColumn += horizontalSpacing
        val verticalCenteringOffset = (layerHeight - vertexInfo.boxRegion.height) / 2
        v -> vertexInfo.setLeft(oldLeftColumn).down(verticalCenteringOffset)
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
    val edgeBendCalculator = new EdgeBendCalculator(edgeInfos, edgeZoneTopRow, currentLayerInfo.topSelfEdgeBuffer)

    val edgeInfoToPoints: Map[EdgeInfo, List[Point]] =
      makeMap(edgeInfos, edgeInfo ⇒ getEdgePoints(edgeInfo, edgeBendCalculator, incompleteEdges))

    val updatedIncompleteEdges: Map[DummyVertex, List[Point]] =
      for ((EdgeInfo(_, finishVertex: DummyVertex, _, _, _), points) ← edgeInfoToPoints)
        yield finishVertex -> points

    val updatedLayerInfo = currentLayerInfo.down(edgeBendCalculator.edgeZoneBottomRow + 1)

    val vertexElements = makeVertexElements(updatedLayerInfo)
    val edgeElements = makeEdgeElements(edgeInfoToPoints)
    val selfEdgeElements = updatedLayerInfo.vertexInfos.collect {
      case (realVertex: RealVertex, vertexInfo) ⇒
        val boxRightEdge = vertexInfo.boxRegion.rightColumn
        vertexInfo.selfOutPorts.zip(vertexInfo.selfInPorts).reverse.zipWithIndex map {
          case ((out, in), i) ⇒
            val p1 = out.down(1)
            val p2 = p1.down(i + 1)
            val p3 = p2.right(boxRightEdge - p2.column + i * 2 + 2)
            val p4 = p3.up(vertexInfo.boxRegion.height + 2 * (i + 1) + 1)
            val p5 = p4.left(p4.column - in.column)
            val p6 = in.up(1)
            EdgeDrawingElement(List(p1, p2, p3, p4, p5, p6), false, true)
        }
    }.toList.flatten
    LayerLayoutResult(vertexElements ++ edgeElements ++ selfEdgeElements, updatedLayerInfo, updatedIncompleteEdges)
  }

  private def makeEdgeInfos(edges: List[Edge], previousLayerInfo: LayerInfo, currentLayerInfo: LayerInfo): List[EdgeInfo] =
    for {
      edge @ Edge(v1, v2) ← edges
      previousVertexInfo ← previousLayerInfo.vertexInfo(v1)
      currentVertexInfo ← currentLayerInfo.vertexInfo(v2)
      start = previousVertexInfo.outEdgeToPortMap(edge).down
      finish = currentVertexInfo.inEdgeToPortMap(edge).up // Note that this will be at the wrong row, we'll adjust later
    } yield EdgeInfo(v1, v2, start, finish, edge.reversed)

  private def getEdgePoints(edgeInfo: EdgeInfo, edgeBendCalculator: EdgeBendCalculator, incompleteEdges: Map[DummyVertex, List[Point]]): List[Point] = {
    val EdgeInfo(startVertex, _, start, finish, _) = edgeInfo
    val trueFinish = finish.translate(down = edgeBendCalculator.edgeZoneBottomRow + 1)
    val priorPoints: List[Point] = startVertex match {
      case dv: DummyVertex ⇒ incompleteEdges(dv)
      case _: RealVertex   ⇒ List(start)
    }
    val lastPriorPoint = priorPoints.last
    val edgePoints =
      if (lastPriorPoint.column == trueFinish.column) // No bend required
        priorPoints :+ trueFinish
      else {
        require(edgeInfo.requiresBend, edgeInfo + ", " + priorPoints)
        val bendRow = edgeBendCalculator.bendRow(edgeInfo)
        priorPoints ++ List(lastPriorPoint.withRow(bendRow), trueFinish.withRow(bendRow), trueFinish)
      }
    Point.removeRedundantPoints(edgePoints)
  }

  private def makeEdgeElements(edgeInfoToPoints: Map[EdgeInfo, List[Point]]): List[EdgeDrawingElement] =
    for ((EdgeInfo(_, finishVertex: RealVertex, _, _, reversed), points) ← edgeInfoToPoints.toList)
      yield EdgeDrawingElement(points, reversed, !reversed)

  private def makeSelfEdgeElements(layerInfo: LayerInfo): List[EdgeDrawingElement] =
    layerInfo.vertexInfos.collect {
      case (realVertex: RealVertex, vertexInfo) ⇒
        vertexInfo.selfOutPorts.zip(vertexInfo.selfInPorts).reverse.zipWithIndex map {
          case ((out, in), i) ⇒ makeSelfEdgeElement(vertexInfo, out, in, i)
        }
    }.toList.flatten

  /**
   * Trace out a self loop path around the right side of the box:
   *
   * p5 ╭───╮p4
   * p6 v   │
   *  ╭───╮ │
   *  │ A │ │
   *  ╰─┬─╯ │
   * p1 │   │
   * p2 ╰───╯p3
   *
   * @param selfEdgeIndex -- if there are multiple self edges, they are drawn nested inside. The edges are numbered
   *  starting with those anchored on the rightmost ports, proceeding leftwards as the index increases.
   */
  private def makeSelfEdgeElement(vertexInfo: VertexInfo, outPort: Point, inPort: Point, selfEdgeIndex: Int): EdgeDrawingElement = {
    val boxRightEdge = vertexInfo.boxRegion.rightColumn
    val p1 = outPort.down(1)
    val p2 = p1.down(selfEdgeIndex + 1)
    val p3 = p2.right(boxRightEdge - p2.column + selfEdgeIndex * 2 + 2)
    val p4 = p3.up(vertexInfo.boxRegion.height + 2 * (selfEdgeIndex + 1) + 1)
    val p5 = p4.left(p4.column - inPort.column)
    val p6 = inPort.up(1)
    EdgeDrawingElement(List(p1, p2, p3, p4, p5, p6), false, true)
  }

  private def makeVertexElements(layerInfo: LayerInfo): List[VertexDrawingElement] =
    layerInfo.realVertexInfos.map {
      case (realVertex, info) ⇒
        val text = getText(vertexRenderingStrategy, realVertex, info.contentRegion.dimension)
        VertexDrawingElement(info.boxRegion, text)
    }

  private def getPreferredSize[V](vertexRenderingStrategy: VertexRenderingStrategy[V], realVertex: RealVertex): Dimension = {
    val preferredSize = vertexRenderingStrategy.getPreferredSize(realVertex.contents.asInstanceOf[V])
    if (vertical)
      preferredSize
    else
      preferredSize.transpose
  }

  private def getText[V](vertexRenderingStrategy: VertexRenderingStrategy[V], realVertex: RealVertex, preferredSize: Dimension) = {
    val actualPreferredSize = if (vertical) preferredSize else preferredSize.transpose
    vertexRenderingStrategy.getText(realVertex.contents.asInstanceOf[V], actualPreferredSize)
  }

}