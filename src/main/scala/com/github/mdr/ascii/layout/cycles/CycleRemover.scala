package com.github.mdr.ascii.layout.cycles

import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import com.github.mdr.ascii.graph.Graph

object CycleRemover {

  def removeCycles[V](graph: Graph[V]): (Graph[V], List[(V, V)]) =
    new CycleRemover().removeCycles(graph)

  def removeSelfLoops[V](graph: Graph[V]): Graph[V] =
    new CycleRemover().removeSelfLoops(graph)

}

class CycleRemover[V] {

  private def findVertexSequence(graph: Graph[V]): List[V] = {
    val db = new CycleRemovalInfo(graph)
    var left: List[V] = Nil
    var right: List[V] = Nil

    @tailrec
    def processSinks() {
      val sinks = db.getSinks
      if (sinks.nonEmpty) {
        for (v ← sinks) {
          db.removeVertex(v)
          right ::= v
        }
        processSinks()
      }
    }

    @tailrec
    def processSources() {
      val sources = db.getSources
      if (sources.nonEmpty) {
        for (v ← sources) {
          db.removeVertex(v)
          left ::= v
        }
        processSources()
      }
    }

    var continue = true
    while (continue) {
      processSinks()
      processSources()

      db.getLargestDegreeDiffVertex match {
        case Some(v) ⇒
          db.removeVertex(v)
          left ::= v
        case None ⇒
          continue = false
      }
    }

    left.reverse ++ right
  }

  def removeSelfLoops(graph: Graph[V]): Graph[V] =
    graph.copy(edges = graph.edges.filterNot { case (v1, v2) ⇒ v1 == v2 })

  /**
   * @return graph without cycles and list of reversed edges (in the new graph). (Note: also removes self-loops, for now).
   */
  def removeCycles(graph: Graph[V]): (Graph[V], List[(V, V)]) = {
    reflowGraph(graph, findVertexSequence(graph))
  }

  /**
   * Given an ordering of the vertices of the graph, return a new graph with all edges that go against
   * the flow of that ordering reversed, together with a list of edges (in the new graph) that have been reversed.
   *
   * (Multiple edges between the same pair of vertices are handled by returning multiple reversed edges)
   */
  def reflowGraph(graph: Graph[V], vertexSequence: List[V]): (Graph[V], List[(V, V)]) = {
    val vertexIndexMap: Map[V, Int] = vertexSequence.zipWithIndex.toMap

    var newEdges: List[(V, V)] = Nil
    var reversedEdges: List[(V, V)] = Nil
    for {
      (source, target) ← graph.edges
      sourceIndex = vertexIndexMap(source)
      targetIndex = vertexIndexMap(target)
    } if (targetIndex < sourceIndex) {
      reversedEdges ::= (target, source)
      newEdges ::= (target, source)
    } else
      newEdges ::= (source, target)

    (graph.copy(edges = newEdges), reversedEdges)
  }

}
