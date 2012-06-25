package com.github.mdr.ascii.layout

import scala.collection.mutable.ListBuffer

case class Graph[V](vertices: List[V], edges: List[(V, V)]) {

  val outMap = edges.groupBy(_._1).map { case (k, vs) ⇒ (k, vs.map(_._2)) }

  val inMap = edges.groupBy(_._2).map { case (k, vs) ⇒ (k, vs.map(_._1)) }

  require(outMap.keys.forall(vertices.contains))
  require(inMap.keys.forall(vertices.contains))

  def inVertices(v: V): List[V] = inMap.getOrElse(v, Nil)

  def outVertices(v: V): List[V] = outMap.getOrElse(v, Nil)

}

class LayeringCalculator[V] {

  def calculateLongestDistances(graph: Graph[V]): Map[V, Int] = {

    var longestDistancesToSink: Map[V, Int] = Map()

    def isSink(v: V) = graph.outVertices(v).isEmpty
    val sinks = graph.vertices.filter(isSink)
    for (sink ← sinks)
      longestDistancesToSink += sink -> 0

    var finalisedVertices: Set[V] = sinks.toSet
    var boundary = finalisedVertices

    while (boundary.nonEmpty) {
      var newBoundary: Set[V] = Set()
      for {
        v2 ← boundary
        v1 ← graph.inVertices(v2)
        currentKnownLongestToSink = longestDistancesToSink.getOrElse(v1, 0)
      } {
        longestDistancesToSink += v1 -> math.max(currentKnownLongestToSink, longestDistancesToSink(v2) + 1)
        if (graph.outVertices(v1).forall(finalisedVertices)) {
          finalisedVertices += v1
          newBoundary += v1
        }
      }
      boundary = newBoundary
    }
    longestDistancesToSink
  }

  def assignLayers(graph: Graph[V]): Layering = {
    val longestDistancesToSink = calculateLongestDistances(graph)
    val maxLayerNum = longestDistancesToSink.values.max
    def layerNum(v: V): Int = maxLayerNum - longestDistancesToSink(v)

    //    println(longestDistancesToSink)
    //    val rawVertexLayers: Array[List[V]] =
    //          graph.vertices.groupBy(longestDistancesToSink).toList.sortBy(_._1).map(new RealVertex(_._2)).toArray

    val realVertices: Map[V, RealVertex] = graph.vertices.map { v ⇒ v -> new RealVertex(v) }.toMap

    val layers: ListBuffer[ListBuffer[Vertex]] = ListBuffer()
    for (layerNum ← 0 to maxLayerNum)
      layers += ListBuffer[Vertex]()

    for (v ← graph.vertices)
      layers(layerNum(v)) += realVertices(v)

    var edges: List[Edge] = Nil
    for {
      (from, to) ← graph.edges
      fromLayer = layerNum(from)
      toLayer = layerNum(to)
    } {
      val dummies = (fromLayer + 1) to (toLayer - 1) map { layerNum ⇒
        val dummy = new DummyVertex()
        layers(layerNum) += dummy
        dummy
      }
      val vertexChain = realVertices(from) +: dummies :+ realVertices(to)
      for ((v1, v2) ← vertexChain.zip(vertexChain.tail))
        edges ::= new Edge(v1, v2)
    }

    Layering(layers.toList.map(lb ⇒ Layer(lb.toList)), edges)
  }

}