package com.github.mdr.ascii.graph

import scala.util.Random

object RandomGraph {

  def randomGraph(implicit random: Random): Graph[String] = {

    def mkVertex: String = {
      val chars = "abcdef\n"
      //      val chars = "abcdefghijklmnopqrstuvwyx".toUpperCase()
      def mkChar = chars(random.nextInt(chars.length))
      val length = random.nextInt(20) + 1
      //      val length = 1
      List.fill(length)(mkChar).mkString
    }

    val numberOfVertices = random.nextInt(12) + 1

    val vertices = List.fill(numberOfVertices)(mkVertex)

    val numberOfEdges = random.nextInt(numberOfVertices * 2)
    def randomVertex = vertices(random.nextInt(numberOfVertices))
    def randomEdge: (String, String) = (randomVertex, randomVertex)
    val edges = List.fill(numberOfEdges)(randomEdge)

    Graph(vertices.toSet, edges)

  }

}