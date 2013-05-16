package com.github.mdr.ascii.layout.layering

/**
 * Layering is a division of vertices into layers and the edges between them
 *
 * Each edge goes between two adjacent layers
 */
case class Layering(layers: List[Layer], edges: List[Edge]) {

  def edgesInto(layer: Layer): List[Edge] = edges.filter { e ⇒ layer.contains(e.finishVertex) }

}

case class Layer(vertices: List[Vertex]) {

  def contains(v: Vertex) = vertices contains v

  def positionOf(v: Vertex) = vertices indexOf v

}

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