package com.github.mdr.ascii.layout

import scala.util.Random.javaRandomToRandom

import org.scalacheck._
import org.scalacheck.Gen.Params
import org.scalacheck.Prop.forAll

import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.graph.GraphUtils
import com.github.mdr.ascii.layout.cycles.CycleRemover
import com.github.mdr.ascii.util.Utils

object CycleRemoverSpecification extends Properties("CycleRemover") {

  implicit val randomGraphGenerator: Gen[Graph[String]] = Gen { p: Params ⇒ Some(RandomGraph.randomGraph(p.rng)) }

  implicit val arbitraryGraph = Arbitrary(randomGraphGenerator)

  implicit val shrinkGraph = Shrink { g: Graph[String] ⇒
    (for (edge ← g.edges.toStream) yield g.removeEdge(edge)) append
      (for (v ← g.vertices.toStream) yield g.removeVertex(v))
  }

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
