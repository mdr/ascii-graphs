package com.github.mdr.ascii.layout.cycles

import scala.util.Random.javaRandomToRandom

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.Params
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.Shrink

import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.graph.GraphUtils
import com.github.mdr.ascii.util.Utils
import com.github.mdr.ascii.graph.GraphGenerators._

object CycleRemoverSpecification extends Properties("CycleRemover") {

  property("no cycles") = forAll { g: Graph[String] ⇒
    val (dag, _) = CycleRemover.removeCycles(g)
    !GraphUtils.hasCycle(dag)
  }

  property("reversed edges are a sub-multiset of the new edges") = forAll { g: Graph[String] ⇒
    val (dag, reversedEdges) = CycleRemover.removeCycles(g)
    val revEdgeMap = Utils.mkMultiset(reversedEdges)
    val newEdgeMap = Utils.mkMultiset(dag.edges)
    reversedEdges.forall { reversedEdge ⇒
      newEdgeMap.getOrElse(reversedEdge, 0) >= revEdgeMap(reversedEdge)
    }
  }

  property("reversing the reversed edges restores the original graph") = forAll { g: Graph[String] ⇒
    val (dag, reversedEdges) = CycleRemover.removeCycles(g)
    unreverse(dag, reversedEdges) == g
  }

  def unreverse[V](graph: Graph[V], reversedEdges: List[(V, V)]): Graph[V] = {
    val rereversedEdges = reversedEdges.map { case (x, y) ⇒ (y, x) }
    val prunedEdges = reversedEdges.foldLeft(graph.edges)(Utils.removeFirst)
    graph.copy(edges = prunedEdges ++ rereversedEdges)
  }

}
