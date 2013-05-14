package com.github.mdr.ascii.graph

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.Params
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.Shrink
import com.github.mdr.ascii.util.Utils
import com.github.mdr.ascii.graph.GraphGenerators._
import com.github.mdr.ascii.layout.cycles.CycleRemover

object TopologicalSortSpecification extends Properties("Topological sort") {

  property("no out-of-order edges") = forAll(dags) { g: Graph[String] ⇒
    val Some(ordering) = GraphUtils.topologicalSort(g)
    val (_, reversedEdges) = new CycleRemover[String].reflowGraph(g, ordering)
    reversedEdges.isEmpty
  }

  property("must cover all vertices") = forAll(dags) { g: Graph[String] ⇒
    val Some(ordering) = GraphUtils.topologicalSort(g)
    ordering.sorted == g.vertices.toList.sorted
  }

}