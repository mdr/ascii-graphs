package com.github.mdr.ascii.layout

sealed abstract class Vertex
class DummyVertex() extends Vertex
class RealVertex(val text: String) extends Vertex { override def toString = "RealVertex(" + text + ")" }

class Edge(val startVertex: Vertex, val finishVertex: Vertex)
object Edge {
  def unapply(e: Edge) = Some((e.startVertex, e.finishVertex))
}

object Layer {

  def apply(vertices: Vertex*): Layer = apply(vertices.toList)

}

case class Layer(vertices: List[Vertex])

case class Layering(layers: List[Layer], edges: List[Edge]) {

}