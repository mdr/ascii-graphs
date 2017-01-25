package com.github.mdr.ascii.layout

import org.scalatest.{FlatSpec, Matchers}
import com.github.mdr.ascii.graph.Graph

// https://github.com/mdr/ascii-graphs/issues/3
class Issue3InfiniteLoopTest extends FlatSpec with Matchers {

  "Layouter" should "not go into an infinite loop" in {
    val v = Set("1", "2", "3", "7", "9")
    val e = List("1" → "3", "1" → "7", "3" → "2", "3" → "9", "7" → "9", "7" → "2")
    val graph = Graph(vertices = v, edges = e)
    GraphLayout.renderGraph(graph)

    // Note this doesn't loop, but still produces an overlapping edge when rendered
  }

  "Layouter" should "really not go into an infinite loop this time" in {

    val v = Set("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
    val e = List("1" → "2", "1" → "3", "1" → "4", "1" → "6", "1" → "7", "1" → "10",
      "2" → "3", "2" → "4", "2" → "5", "2" → "6", "2" → "7", "2" → "8", "2" → "10",
      "3" → "9", "4" → "5", "4" → "8", "7" → "9")

    val graph = Graph(vertices = v, edges = e)
    GraphLayout.renderGraph(graph)
  }
}

