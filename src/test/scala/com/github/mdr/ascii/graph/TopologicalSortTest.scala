package com.github.mdr.ascii.graph

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers

class TopologicalSortTest extends FlatSpec with ShouldMatchers with Checkers {

  check("""
         +-+   +-+ 
         |A|-->|B| 
         +-+   +-+ """, "A", "B")

  check("""
         +-+   +-+ 
         |A|-->|B| 
         +-+   +-+ 
          |     ^  
          |     |  
          -------  """, "A", "B")

  checkNoSort("""
         +-+   +-+ 
      -->|A|-->|B| 
      |  +-+   +-+ 
      |   |        
      |   |        
      -----        """)

  def checkNoSort(diagram: String) {
    "Topological sorting" should ("not be found: " + diagram) in {
      val graph = Graph.fromDiagram(diagram)
      GraphUtils.topologicalSort(graph) should be(None)
    }
  }

  def check(diagram: String, expectedOrdering: String*) {
    "Topological sorting" should ("work: " + diagram) in {
      val graph = Graph.fromDiagram(diagram)
      val Some(actualOrdering) = GraphUtils.topologicalSort(graph)
      actualOrdering should be(expectedOrdering)
    }
  }

}