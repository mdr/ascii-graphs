package com.github.mdr.ascii.layout.layering

/**
 * Layering is a division of vertices into layers, and edges, which can go between multiple layers.
 *
 * Each edge must finish at a later layer in the layer order than it starts.
 */
case class Layering(layers: List[Layer], edges: List[Edge])

case class Layer(vertices: List[Vertex])

sealed abstract class Vertex

/**
 * A vertex used as a bend point when an edge passes through a layer.
 */
class DummyVertex() extends Vertex {
  override def toString = "DummyVertex"
}

class RealVertex(val contents: Any, val selfEdges: Int) extends Vertex {

  override def toString = "RealVertex(" + contents.toString + ", selfEdges = " + selfEdges + ")"

}

class Edge(val startVertex: Vertex, val finishVertex: Vertex, val reversed: Boolean) {

  override def toString = "Edge(" + startVertex + ", " + finishVertex + "," + reversed + ")"

}

object Edge {

  def unapply(e: Edge) = Some((e.startVertex, e.finishVertex))

}
