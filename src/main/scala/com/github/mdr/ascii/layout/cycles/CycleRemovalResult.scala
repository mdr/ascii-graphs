package com.github.mdr.ascii.layout.cycles

import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.util.Utils

case class CycleRemovalResult[V](dag: Graph[V], reversedEdges: List[(V, V)], selfEdges: List[(V, V)]) {

  def countSelfEdges(v: V): Int = selfEdges.count(_._1 == v)

}
