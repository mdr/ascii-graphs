package com.github.mdr.ascii.graph

/**
 * Parse a Graph from a list of vertices/edges (as produced by Graph.toString)
 */
object GraphFromText {

  def apply(text: String): Graph[String] = {
    val pieces = text.split("\n").toList.map(_.split(",").toList.map(_.replace("\\n", "\n")))
    val edges = pieces.flatMap { chunks ⇒ chunks.zip(chunks.tail) }
    val vertices = if (text.trim.isEmpty) Set[String]() else pieces.flatten.toSet
    val graph = Graph(vertices, edges)
    graph
  }

}