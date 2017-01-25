package com.github.mdr.ascii.layout.layering

import com.github.mdr.ascii.graph.Graph

object LongestDistancesToSinkCalculator {

  /**
   * For each vertex in the given graph (which must be acyclic), calculate the length of the longest path
   * to a sink.
   */
  def longestDistancesToSink[V](graph: Graph[V]): Map[V, Int] = {

    val sinks = graph.sinks

    /**
     * The vertices for which we know the longest distance to a sink.
     */
    var finalisedVertices: Set[V] = graph.sinks.toSet

    /**
     * The current known longest distances to a sink.
     */
    var distances: Map[V, Int] = graph.vertices.map(_ → 0).toMap

    var boundary = finalisedVertices

    while (boundary.nonEmpty) {
      var newBoundary = Set[V]()
      for {
        v2 ← boundary
        v1 ← graph.inVertices(v2)
      } {
        val newDistance = math.max(distances(v1), distances(v2) + 1)
        distances += v1 → newDistance
        if (graph.outVertices(v1).forall(finalisedVertices)) {
          finalisedVertices += v1
          newBoundary += v1
        }
      }
      boundary = newBoundary
    }
    distances
  }

}