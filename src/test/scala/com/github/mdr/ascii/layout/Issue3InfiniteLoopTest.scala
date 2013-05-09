package com.github.mdr.ascii.layout

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

// https://github.com/mdr/ascii-graphs/issues/3
class Issue3InfiniteLoopTest extends FlatSpec with ShouldMatchers {

  "Layouter" should "not go into an infinite loop" in {
    val v = List("1", "2", "3", "7", "9")
    val e = List("1" -> "3", "1" -> "7", "3" -> "2", "3" -> "9", "7" -> "9", "7" -> "2")
    val graph = Graph(vertices = v, edges = e)
    Layouter.renderGraph(graph)
    
    // Note this doesn't loop, but still produces an overlapping edge when rendered
  }
}

