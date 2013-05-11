package com.github.mdr.ascii.layout.layering

case class Layer(vertices: List[Vertex])

case class Layering(layers: List[Layer], edges: List[Edge])

sealed abstract class Vertex

class DummyVertex() extends Vertex {
  override def toString = "DummyVertex"
}

class RealVertex(val contents: Any) extends Vertex {

  override def toString = "RealVertex(" + contents.toString + ")"

}

class Edge(val startVertex: Vertex, val finishVertex: Vertex, val reversed: Boolean) {

  override def toString = "Edge(" + startVertex + ", " + finishVertex + "," + reversed + ")"

}

object Edge {

  def unapply(e: Edge) = Some((e.startVertex, e.finishVertex))

}
