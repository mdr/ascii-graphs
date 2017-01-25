package com.github.mdr.ascii.layout.cycles

import com.github.mdr.ascii.graph.{Graph, GraphUtils}
import com.github.mdr.ascii.graph.GraphGenerators._
import com.github.mdr.ascii.util.Utils
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object CycleRemoverSpecification extends Properties("CycleRemover") {

  property("no cycles") = forAll { g: Graph[String] ⇒
    val CycleRemovalResult(dag, _, _) = CycleRemover.removeCycles(g)
    !GraphUtils.hasCycle(dag)
  }

  property("reversed edges are a sub-multiset of the new edges") = forAll { g: Graph[String] ⇒
    val CycleRemovalResult(dag, reversedEdges, _) = CycleRemover.removeCycles(g)
    val revEdgeMap = Utils.mkMultiset(reversedEdges)
    val newEdgeMap = Utils.mkMultiset(dag.edges)
    reversedEdges.forall { reversedEdge ⇒
      newEdgeMap.getOrElse(reversedEdge, 0) >= revEdgeMap(reversedEdge)
    }
  }

  property("reversing the reversed edges restores the original graph") = forAll { g: Graph[String] ⇒
    recoverOriginal(CycleRemover.removeCycles(g)) == g
  }

  property("self-edges are correct") = forAll { g: Graph[String] ⇒
    val selfEdges = CycleRemover.removeCycles(g).selfEdges
    selfEdges.forall { case (v1, v2) ⇒ v1 == v2 }
    val selfEdgesMap = Utils.mkMultiset(selfEdges)
    val allEdgesMap = Utils.mkMultiset(g.edges)
    selfEdges.forall { selfEdge ⇒
      allEdgesMap.getOrElse(selfEdge, 0) >= selfEdgesMap(selfEdge)
    }
  }

  def recoverOriginal[V](result: CycleRemovalResult[V]): Graph[V] = {
    val CycleRemovalResult(dag, reversedEdges, selfEdges) = result
    val rereversedEdges = reversedEdges.map { case (x, y) ⇒ (y, x) }
    val prunedEdges = reversedEdges.foldLeft(dag.edges)(Utils.removeFirst)
    dag.copy(edges = prunedEdges ++ rereversedEdges ++ selfEdges)
  }

}
