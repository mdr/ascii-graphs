package com.github.mdr.ascii.perf

import com.github.mdr.ascii.diagram.Diagram
import com.github.mdr.ascii.util.Utils
import com.github.mdr.ascii.graph.Graph

object PerfTest extends App {

  val text = Utils.getResourceAsString("/large2.graph")
  val start1 = System.currentTimeMillis
  val graph = Graph.fromDiagram(text)
  val duration1 = System.currentTimeMillis - start1
  println("Parsed")
  println("Vertex = " + graph.vertices.size)
  println("Edges = " + graph.edges.size)
  println("Parse duration = " + duration1 + " ms")

  val start = System.currentTimeMillis
  val s = graph.toString
  val duration = System.currentTimeMillis - start
  println(s)
  println("Layout duration = " + duration + " ms")

}