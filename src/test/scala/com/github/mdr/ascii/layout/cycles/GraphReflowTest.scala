package com.github.mdr.ascii.layout.cycles

import org.scalatest.{FlatSpec, Matchers}
import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.util.Utils

import scala.language.reflectiveCalls

class GraphReflowTest extends FlatSpec with Matchers {

  reflowingGraph("""
         +-+   +-+ 
         |A|-->|B| 
         +-+   +-+ """).
    inOrder("A", "B").
    shouldProduce("""
         +-+   +-+ 
         |A|-->|B| 
         +-+   +-+ """).
    havingReversedEdges()

  reflowingGraph("""
         +-+   +-+ 
         |A|<--|B| 
         +-+   +-+ """).
    inOrder("A", "B").
    shouldProduce("""
         +-+   +-+ 
         |A|-->|B| 
         +-+   +-+ """).
    havingReversedEdges("A" → "B")

  reflowingGraph("""
         +-+   +-+   +-+ 
         |A|-->|B|-->|C| 
         +-+   +-+   +-+ 
          ^           |  
          |           |  
          -------------  """).
    inOrder("C", "B", "A").
    shouldProduce("""
         +-+   +-+   +-+ 
         |A|<--|B|<--|C| 
         +-+   +-+   +-+ 
          ^           |  
          |           |  
          -------------  """).
    havingReversedEdges("B" → "A", "C" → "B")

  reflowingGraph("""
         +-+   +-+ 
         |A|-->|C| 
         +-+   +-+ 
          ^     |  
          |     |  
          -------  """).
    inOrder("A", "C").
    shouldProduce("""
         +-+   +-+ 
         |A|-->|C| 
         +-+   +-+ 
          |     ^  
          |     |  
          -------  """).
    havingReversedEdges("A" → "C")

  reflowingGraph("""
          +-+   +-+     
       -->|A|-->|C|---  
       |  +-+   +-+  |  
       |   ^     |   |  
       |   |     |   |  
       |   -------   |  
       ---------------  """).
    inOrder("A", "C").
    shouldProduce("""
          +-+   +-+     
       ---|A|-->|C|<--  
       |  +-+   +-+  |  
       |   |     ^   |  
       |   |     |   |  
       |   -------   |  
       ---------------  """).
    havingReversedEdges("A" → "C", "A" → "C")

  reflowingGraph("""
               +-+ 
          ---->|A| 
          |    +-+ 
          |     |  
          |     |  
          -------  """).
    inOrder("A").
    shouldProduce("""
               +-+ 
          ---->|A| 
          |    +-+ 
          |     |  
          |     |  
          -------  """).
    havingReversedEdges()

  reflowingGraph("""
          +-+ 
          |B| 
          +-+ 
           """).
    inOrder("B").
    shouldProduce("""
          +-+ 
          |B| 
          +-+ 
           """).
    havingReversedEdges()

  reflowingGraph("").inOrder().shouldProduce("").havingReversedEdges()

  def check(inputGraph: Graph[String], order: List[String], expectedGraph: Graph[String], expectedReversedEdges: List[(String, String)]) {
    "Reflowing graphs" should ("work on graph " + inputGraph) in {
      val (reflowedGraph, reversedEdges) = new CycleRemover[String].reflowGraph(inputGraph, order)
      Utils.multisetCompare(reversedEdges, expectedReversedEdges) should be(true)
      reversedEdges.toSet should equal(expectedReversedEdges.toSet)
      reflowedGraph should equal(expectedGraph)
    }
  }

  def reflowingGraph(inputGraphString: String) = new {
    def inOrder(vertices: String*) = new {
      def shouldProduce(expectedGraphString: String) = new {
        def havingReversedEdges(expectedReversedEdges: (String, String)*) {
          val inputGraph = Graph.fromDiagram(inputGraphString)
          val expectedGraph = Graph.fromDiagram(expectedGraphString)
          check(inputGraph, vertices.toList, expectedGraph, expectedReversedEdges.toList)
        }
      }
    }

  }

}