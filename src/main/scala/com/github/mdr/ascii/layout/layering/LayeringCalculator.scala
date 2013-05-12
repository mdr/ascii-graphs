package com.github.mdr.ascii.layout.layering

import scala.collection.mutable.ListBuffer
import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.layout.cycles.CycleRemovalResult
import com.github.mdr.ascii.util.Utils

class LayeringCalculator[V] {

  /**
   * For each vertex in the given graph (which must be acyclic), calculate the length of the longest path
   * to a sink.
   */
  def longestDistancesToSink(graph: Graph[V]): Map[V, Int] = {

    val sinks = graph.sinks

    /**
     * The vertices for which we know the longest distance to a sink.
     */
    var finalisedVertices: Set[V] = graph.sinks.toSet

    /**
     * The current known longest distances to a sink.
     */
    var distances: Map[V, Int] = graph.vertices.map(_ -> 0).toMap

    var boundary = finalisedVertices

    while (boundary.nonEmpty) {
      var newBoundary = Set[V]()
      for {
        v2 ← boundary
        v1 ← graph.inVertices(v2)
      } {
        val newDistance = math.max(distances(v1), distances(v2) + 1)
        distances += v1 -> newDistance
        if (graph.outVertices(v1).forall(finalisedVertices)) {
          finalisedVertices += v1
          newBoundary += v1
        }
      }
      boundary = newBoundary
    }
    distances
  }

  private def makeRealVertices(cycleRemovalResult: CycleRemovalResult[V]): Map[V, RealVertex] =
    cycleRemovalResult.dag.vertices.map { v ⇒
      val selfEdges = cycleRemovalResult.countSelfEdges(v)
      v -> new RealVertex(v, selfEdges)
    }.toMap

  def assignLayers(cycleRemovalResult: CycleRemovalResult[V]): Layering = {
    val graph = cycleRemovalResult.dag

    val distancesToSink = longestDistancesToSink(graph)
    val maxLayerNum = if (graph.isEmpty) 0 else distancesToSink.values.max
    def layerNum(v: V): Int = maxLayerNum - distancesToSink(v)

    val layers: ListBuffer[ListBuffer[Vertex]] = ListBuffer()
    for (layerNum ← 0 to maxLayerNum)
      layers += ListBuffer[Vertex]()

    val realVertices: Map[V, RealVertex] = makeRealVertices(cycleRemovalResult)
    for (v ← graph.vertices)
      layers(layerNum(v)) += realVertices(v)

    // We decrement counts in revEdges as we construct layer edges to make sure the right number of reversed edges
    // are generated:
    var revEdges = Utils.mkMultiset(cycleRemovalResult.reversedEdges)
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