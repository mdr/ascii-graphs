package com.github.mdr.ascii.layout

import com.github.mdr.ascii._

sealed abstract class Vertex
class DummyVertex() extends Vertex
class RealVertex(val text: String) extends Vertex

class Layer {

  def vertices: List[Vertex] = ???

}

class Edge {

  def vertex1: Vertex = ???

  def vertex2: Vertex = ???

  def arrow1: Boolean = ???

  def arrow2: Boolean = ???

}

class Layering {

  def layers: List[Layer] = ???

}

object Layouter {

  case class VertexInfo(region: Region, inPorts: Map[Vertex, Point], outPorts: Map[Vertex, Point]) {

  }

  private var vertexInfos: Map[Vertex, VertexInfo] = Map()

  private def calculateLayerInfo(vertices: List[Vertex], inEdges: List[(Vertex, Vertex)], outEdges: List[(Vertex, Vertex)], row: Int) {
    def inVertices(vertex: Vertex) = inEdges collect { case (v1, `vertex`) ⇒ v1 }
    def outVertices(vertex: Vertex) = outEdges collect { case (`vertex`, v2) ⇒ v2 }
    val dimensions: Map[Vertex, Dimension] =
      (for {
        vertex ← vertices
        outDegree = outVertices(vertex).size
        inDegree = inVertices(vertex).size
        width = math.max(math.max(3, outDegree * 2 + 3), inDegree * 2 + 3)
        dimension = Dimension(height = 3, width = width)
      } yield vertex -> dimension).toMap

    var regions: Map[Vertex, Region] = Map()
    var pos = Point(row, 0)
    for (vertex ← vertices) {
      val region = Region(pos, dimensions(vertex))
      regions += vertex -> region
      pos = region.topRight.right(3)
    }

    for (vertex ← vertices) {
      val region = regions(vertex)
      val point = region.bottomLeft

      val inPorts = (for ((vertex, index) ← inVertices(vertex).zipWithIndex)
        yield vertex -> region.topLeft.right(index * 2 + 2)).toMap
      val outPorts = (for ((vertex, index) ← outVertices(vertex).zipWithIndex)
        yield vertex -> region.bottomLeft.right(index * 2 + 2)).toMap
      val vertexInfo = VertexInfo(region, inPorts, outPorts)
      vertexInfos += vertex -> vertexInfo
    }

  }

  def layout(vertices1: List[Vertex], vertices2: List[Vertex], edges: List[(Vertex, Vertex)]): List[DrawingElement] = {
    calculateLayerInfo(vertices1, Nil, edges.sortBy { case (_, v2) ⇒ vertices2.indexOf(v2) }, 0)
    calculateLayerInfo(vertices2, edges.sortBy { case (v1, _) ⇒ vertices1.indexOf(v1) }, Nil, 1 + edges.size  * 2)

    var x = 0
    val edgeEls = for {
      ((v1, v2), x2) ← edges.zipWithIndex
      start = vertexInfos(v1).outPorts(v2).down
      finish = vertexInfos(v2).inPorts(v1).up
    } yield {
      val points =
        if (start.column == finish.column)
          List(start, finish)
        else {
          val horizRow = start.down(x + 1).row
          x += 2
          List(start, start.copy(row = horizRow), finish.copy(row = horizRow), finish)
        }
      EdgeDrawingElement(points.distinct, false, true)
    }

    vertexInfos.toList.flatMap {
      case (vertex: RealVertex, info) ⇒
        VertexDrawingElement(info.region, List(vertex.text)) :: Nil
    } ++ edgeEls
  }

}