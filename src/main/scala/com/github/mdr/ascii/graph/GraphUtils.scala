package com.github.mdr.ascii.graph

object GraphUtils {

  def topologicalSort[V](g: Graph[V]): Option[List[V]] = {
    var sort: List[V] = Nil
    var sources: List[V] = g.sources
    var deletedEdges: Set[(V, V)] = Set()
    while (sources.nonEmpty) {
      val (n :: rest) = sources
      sources = rest
      sort ::= n
      for (m ← g.outVertices(n) if !deletedEdges.contains((m, n))) {
        deletedEdges += ((m, n))
        if (g.inEdges(m).filterNot(deletedEdges).isEmpty)
          sources ::= m
      }
    }
    if (deletedEdges == g.edges)
      Some(sort)
    else
      None
  }

  def hasCycle(g: Graph[_]): Boolean = topologicalSort(g).isDefined

}
