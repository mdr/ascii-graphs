package com.github.mdr.ascii.layout

object CycleRemover {

  def findVertexSequence[V](graph: Graph[V]): List[V] = {

    Nil
  }

  /**
   * @return graph without cycles and list of reversed edges.
   */
  def removeCycles[V](graph: Graph[V], vertexSequence: List[V]): (Graph[V], List[(V, V)]) = {
    var newEdges: List[(V, V)] = Nil
    var reversedEdges: List[(V, V)] = Nil
    for {
      (vertex, index) ← vertexSequence.zipWithIndex
      outVertex ← graph.outVertices(vertex)
    } {
      val outVertexIndex = vertexSequence.indexOf(outVertex).ensuring(_ >= 0)
      if (outVertexIndex < index) {
        reversedEdges ::= (outVertex, vertex)
        newEdges ::= (outVertex, vertex)
      } else
        newEdges ::= (vertex, outVertex)
    }
    (graph.copy(edges = newEdges), reversedEdges)
  }

}