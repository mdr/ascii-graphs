package com.github.mdr.ascii.layout

import scala.collection.mutable.ListBuffer
import com.github.mdr.ascii.graph.Graph

class LayeringCalculator[V] {

  private def calculateLongestDistances(graph: Graph[V]): Map[V, Int] = {

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

  def assignLayers(graph: Graph[V], reversedEdges: Map[(V, V), Int]): Layering = {
    val longestDistancesToSink = calculateLongestDistances(graph)
    val maxLayerNum = if (graph.isEmpty) 0 else longestDistancesToSink.values.max
    def layerNum(v: V): Int = maxLayerNum - longestDistancesToSink(v)

    val realVertices: Map[V, RealVertex] = graph.vertices.map { v ⇒ v -> new RealVertex(v) }.toMap

    val layers: ListBuffer[ListBuffer[Vertex]] = ListBuffer()
    for (layerNum ← 0 to maxLayerNum)
      layers += ListBuffer[Vertex]()

    for (v ← graph.vertices)
      layers(layerNum(v)) += realVertices(v)

    // We decrement counts in revEdges as we construct layer edges to make sure the right proportion of reversed edges
    // are generated:
    var revEdges = reversedEdges
    var edges: List[Edge] = Nil
    for {
      graphEdge @ (from, to) ← graph.edges
      fromLayer = layerNum(from)
      toLayer = layerNum(to)
    } {
      val dummies = (fromLayer + 1) to (toLayer - 1) map { layerNum ⇒
        val dummy = new DummyVertex
        layers(layerNum) += dummy
        dummy
      }
      val vertexChain = realVertices(from) +: dummies :+ realVertices(to)
      val reversed = revEdges.get(graphEdge) match {
        case Some(count) ⇒
          if (count == 1)
            revEdges -= graphEdge
          else
            revEdges += graphEdge -> (count - 1)
          true
        case None ⇒
          false
      }
      for ((v1, v2) ← vertexChain.zip(vertexChain.tail))
        edges ::= new Edge(v1, v2, reversed)
    }

    Layering(layers.toList.map(lb ⇒ Layer(lb.toList)), edges)
  }

}