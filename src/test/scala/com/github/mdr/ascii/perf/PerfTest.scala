package com.github.mdr.ascii.perf

import com.github.mdr.ascii.diagram.Diagram
import com.github.mdr.ascii.util.Utils
import com.github.mdr.ascii.graph.Graph

object PerfTest extends App {

  val graph = Graph.fromDiagram(Utils.getResourceAsString("/large1.graph"))
  val start = System.currentTimeMillis
  val s = graph.toString()
  val duration = System.currentTimeMillis - start
  println(s)
  println(duration + "ms")

}