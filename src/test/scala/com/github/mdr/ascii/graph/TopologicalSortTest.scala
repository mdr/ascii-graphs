package com.github.mdr.ascii.graph

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TopologicalSortTest extends FlatSpec with ShouldMatchers {

  check("""
         +-+   +-+ 
         |A|-->|B| 
         +-+   +-+ """, "A", "B")

  def check(diagram: String, expectedOrdering: String*) {
    "Topological sorting" should ("work: " + diagram) in {
      val graph = Graph.fromDiagram(diagram)
      val Some(actualOrdering) = GraphUtils.topologicalSort(graph)
      actualOrdering should be(expectedOrdering)
    }
  }

}