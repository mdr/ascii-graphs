package com.github.mdr.ascii.layout

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.graph.GraphUtils
import com.github.mdr.ascii.layout.cycles.CycleRemover
import com.github.mdr.ascii.util.Utils

class CycleRemoverTest extends FlatSpec with ShouldMatchers {

  check("""
         +-+   +-+ 
         |A|-->|B| 
         +-+   +-+ """)

  check("""
         +-+   +-+ 
         |A|-->|B| 
         +-+   +-+ 
          ^     |  
          |     |  
          -------  """)

  check("""
         +-+   +-+   +-+ 
         |A|-->|B|-->|C| 
         +-+   +-+   +-+ 
          ^           |  
          |           |  
          -------------  """)

  check("""
          +-+   +-+     
       -->|A|-->|C|---  
       |  +-+   +-+  |  
       |   ^     |   |  
       |   |     |   |  
       |   -------   |  
       ---------------  """)

  check("""
               +-+ 
          ---->|A| 
          |    +-+ 
          |     |  
          |     |  
          -------  """)

  check("""
          +-+ 
          |B| 
          +-+ 
           """)

  check("")

  def check(diagram: String) {
    val graph = Graph.fromDiagram(diagram)
    "Graph" should ("not have cycles " + ">>>\n" + graph + "\n<<<") in {
      val (newGraph, reversedEdges) = CycleRemover.removeCycles(graph)

      GraphUtils.hasCycle(newGraph) should be(false)

      val revEdgeMap = Utils.mkMultiset(reversedEdges)
      val newEdgeMap = Utils.mkMultiset(newGraph.edges)
      for (reversedEdge ← reversedEdges)
        newEdgeMap.getOrElse(reversedEdge, 0) should be >= (revEdgeMap(reversedEdge))

      CycleRemoverSpecification.unreverse(newGraph, reversedEdges) should be(graph)
    }
  }

}

