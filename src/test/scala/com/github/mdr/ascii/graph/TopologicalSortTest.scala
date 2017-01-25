package com.github.mdr.ascii.graph

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.Checkers

class TopologicalSortTest extends FlatSpec with Matchers with Checkers {

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
    val graph = Graph.fromDiagram(diagram)
    "Topological sorting" should ("not be found: " + graph) in {
      GraphUtils.topologicalSort(graph) should be(None)
    }
  }

  def check(diagram: String, expectedOrdering: String*) {
    val graph = Graph.fromDiagram(diagram)
    "Topological sorting" should ("work: " + graph) in {
      val Some(actualOrdering) = GraphUtils.topologicalSort(graph)
      actualOrdering should be(expectedOrdering)
    }
  }

}