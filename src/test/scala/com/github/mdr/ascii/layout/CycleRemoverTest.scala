package com.github.mdr.ascii.layout

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import com.github.mdr.ascii.layout.cycles.CycleRemover
import scala.language.reflectiveCalls
import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.graph.GraphUtils
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

      unreverse(newGraph, reversedEdges) should be(graph)
    }
  }

  def removeFirst[T](xs: List[T], x: T): List[T] = xs match {
    case Nil    ⇒ Nil
    case h :: t ⇒ if (h == x) t else h :: removeFirst(t, x)
  }

  def unreverse[V](graph: Graph[V], reversedEdges: List[(V, V)]): Graph[V] = {
    val rereversedEdges = reversedEdges.map { case (x, y) ⇒ (y, x) }
    val prunedEdges = reversedEdges.foldLeft(graph.edges)(removeFirst)
    graph.copy(edges = prunedEdges ++ rereversedEdges)
  }
}

