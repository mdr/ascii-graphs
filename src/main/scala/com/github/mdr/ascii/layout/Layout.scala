package com.github.mdr.ascii.layout

import com.github.mdr.ascii._

sealed abstract class Vertex
class DummyVertex() extends Vertex
class RealVertex(val text: String) extends Vertex { override def toString = "RealVertex(" + text  + ")" }

//class Layer {
//
//  def vertices: List[Vertex] = ???
//
//}
//
//class Edge {
//
//  def vertex1: Vertex = ???
//
//  def vertex2: Vertex = ???
//
//  def arrow1: Boolean = ???
//
//  def arrow2: Boolean = ???
//
//}
//
//class Layering {
//
//  def layers: List[Layer] = ???
//
//}

object Layouter {

  private case class VertexInfo(region: Region, inPorts: Map[Edge, Point], outPorts: Map[Edge, Point]) {

  }

  private class Edge(val startVertex: Vertex, val finishVertex: Vertex)
  private object Edge {
    def unapply(e: Edge) = Some((e.startVertex, e.finishVertex))
  }

  private var vertexInfos: Map[Vertex, VertexInfo] = Map()

  val VERTEX_HEIGHT = 3

  private def calculateVertexInfo(vertices: List[Vertex], inEdges: List[Edge], outEdges: List[Edge], topLeftRow: Int): Map[Vertex, VertexInfo] = {
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
    var pos = Point(topLeftRow, 0)
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

  def layout(vertices1: List[Vertex], vertices2: List[Vertex], vertices3: List[Vertex], edgePairs: List[(Vertex, Vertex)]): List[DrawingElement] = {

    val edges = edgePairs.map { case (v1, v2) ⇒ new Edge(v1, v2) }
    vertexInfos = vertexInfos ++ calculateVertexInfo(vertices1, Nil, edges.sortBy { case Edge(_, v2) ⇒ vertices2.indexOf(v2) }, 0)
    vertexInfos = vertexInfos ++ calculateVertexInfo(vertices2, edges.sortBy { case Edge(v1, _) ⇒ vertices1.indexOf(v1) }, edges.sortBy { case Edge(_, v2) ⇒ vertices3.indexOf(v2) }, 15) //5 + edges.size * 2
//    vertexInfos = vertexInfos ++ calculateVertexInfo(vertices3, edges.sortBy { case Edge(v1, _) ⇒ vertices1.indexOf(v1) }, Nil, 30)

    val edgeInfos =
      for {
        edge @ Edge(v1, v2) ← edges
        start = vertexInfos(v1).outPorts(edge).down
        finish = vertexInfos(v2).inPorts(edge).up
      } yield EdgeInfo(v1, v2, start, finish)

    // We sort this way to avoid unnecessary overlaps coming into the same vertex
    val sortedInfos = edgeInfos.sortBy { info ⇒
      val diff = info.startPort.column - info.finishPort.column
      val sign = if (diff == 0) 0 else diff / math.abs(diff)
      sign * info.finishPort.column
    }

    var edgeRows: Map[EdgeInfo, Int] = Map()
    var rowOffset = 0
    for { edgeInfo @ EdgeInfo(_, _, startPort, finishPort) ← sortedInfos } {
      if (startPort.column != finishPort.column) {
        edgeRows += edgeInfo -> startPort.down(rowOffset + 1).row
        rowOffset += 2
      }
    }
    edgeRows.foreach(println)

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

    val edgeElements =
      for (edgeInfo @ EdgeInfo(_, _, start, finish) ← sortedInfos) yield {
        val points =
          if (start.column == finish.column)
            List(start, finish) // No bend required
          else {
            val horizRow = edgeRows(edgeInfo)
            List(start, start.copy(row = horizRow), finish.copy(row = horizRow), finish)
          }
        EdgeDrawingElement(points.distinct, false, true)
      }

    vertexInfos.toList.collect {
      case (vertex: RealVertex, info) ⇒ VertexDrawingElement(info.region, List(vertex.text))
    } ++ edgeElements
  }

}