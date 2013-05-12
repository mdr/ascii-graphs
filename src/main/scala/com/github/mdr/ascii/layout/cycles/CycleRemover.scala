package com.github.mdr.ascii.layout.cycles

import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import com.github.mdr.ascii.graph.Graph

object CycleRemover {

  /**
   * Remove cycles and self loops from the given graph, resulting in a DAG. Removed self-loops and reversed
   * edges are also returned.
   */
  def removeCycles[V](graph: Graph[V]): CycleRemovalResult[V] = {
    val cycleRemover = new CycleRemover[V]
    val (graphWithoutLoops, selfEdges) = cycleRemover.removeSelfLoops(graph)
    val (graphWithoutCycles, reversedEdges) = cycleRemover.removeCycles(graphWithoutLoops)
    CycleRemovalResult(graphWithoutCycles, reversedEdges, selfEdges)
  }

}

class CycleRemover[V] {

  private class Removal(graph: Graph[V]) {

    private val graphInfo = new CycleRemovalInfo(graph)

    private var left: List[V] = Nil
    private var right: List[V] = Nil

    @tailrec
    final def run(): Removal = {
      addSinksToRight()
      addSourcesToLeft()

      graphInfo.getLargestDegreeDiffVertex match {
        case Some(v) ⇒
          graphInfo.removeVertex(v)
          left ::= v
          run()
        case None ⇒
          this
      }
    }

    private def addSinksToRight() =
      while (graphInfo.getSinks.nonEmpty)
        for (v ← graphInfo.getSinks) {
          graphInfo.removeVertex(v)
          right ::= v
        }

    private def addSourcesToLeft() =
      while (graphInfo.getSources.nonEmpty)
        for (v ← graphInfo.getSources) {
          graphInfo.removeVertex(v)
          left ::= v
        }

    def getSequence = left.reverse ++ right

  }

  private def findVertexSequence(graph: Graph[V]): List[V] =
    new Removal(graph).run().getSequence

  private def isSelfLoop[V](edge: (V, V)) = edge._1 == edge._2

  /**
   * @return a graph without self-loops, together with a list of the self-edges that were removed.
   */
  def removeSelfLoops(graph: Graph[V]): (Graph[V], List[(V, V)]) = {
    val (selfEdges, newEdges) = graph.edges.partition(isSelfLoop)
    (graph.copy(edges = newEdges), selfEdges)
  }

  /**
   * @return graph without cycles and list of reversed edges (in the new graph).
   */
  def removeCycles(graph: Graph[V]): (Graph[V], List[(V, V)]) =
    reflowGraph(graph, findVertexSequence(graph))

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
