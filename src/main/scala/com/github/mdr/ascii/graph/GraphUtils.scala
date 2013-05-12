package com.github.mdr.ascii.graph

object GraphUtils {

  /**
   * @return Some(vertices), an ordering of vertices in the given graph such that, if there is an edge from
   * u to v, then u comes before v in the order. If such an ordering does not exist, then None is returned.
   */
  def topologicalSort[V](g: Graph[V]): Option[List[V]] = {
    var sort: List[V] = Nil
    var sources: List[V] = g.sources
    var deletedEdges: Set[(V, V)] = Set()
    while (sources.nonEmpty) {
      val (n :: rest) = sources
      sources = rest
      sort ::= n
      for (m ← g.outVertices(n) if !deletedEdges.contains((m, n))) {
        deletedEdges += ((n, m))
        if (g.inEdges(m).filterNot(deletedEdges).isEmpty && !sources.contains(m) /* <- because multi-edges */ )
          sources ::= m
      }
    }
    if (deletedEdges == g.edges.toSet)
      Some(sort.reverse)
    else
      None
  }

  def hasCycle(g: Graph[_]): Boolean = topologicalSort(g).isEmpty

}
