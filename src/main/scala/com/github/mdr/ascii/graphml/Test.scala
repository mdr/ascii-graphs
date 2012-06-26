package com.github.mdr.ascii.graphml

import java.io.FileInputStream

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

import com.github.mdr.ascii.layout.Graph
import com.github.mdr.ascii.layout.LayerOrderingCalculator
import com.github.mdr.ascii.layout.LayeringCalculator
import com.github.mdr.ascii.layout.Layouter
import com.github.mdr.ascii.layout.Renderer
import com.github.mdr.ascii.layout.ToStringVertexRenderingStrategy
import com.tinkerpop.blueprints
import com.tinkerpop.blueprints.Direction
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import com.tinkerpop.blueprints.util.io.graphml.GraphMLReader

object Test extends App {

  private def convertGraph(graph: blueprints.Graph): Graph[String] = {
    def makeVertex(v: Vertex) = {
      val s = v.getId.toString
      val List(a, b, c) = s.split(":").toList
      List(b, a, c).mkString("\n")
    }

    for (v ← graph.getVertices) {
      if (v.getId.toString.contains("com.example") || v.getId.toString.contains("org.scala-lang")) {
        graph.removeVertex(v)
      }
    }

    val vertices = graph.getVertices.asScala.toList.map(makeVertex)

    val edges = graph.getEdges.asScala.toList.map { edge ⇒
      (makeVertex(edge.getVertex(Direction.IN)), makeVertex(edge.getVertex(Direction.OUT)))
    }

    Graph(vertices, edges)
  }

  private def readGraph(file: String): blueprints.Graph = {
    val graph = new TinkerGraph
    val inputStream = new FileInputStream(file)
    GraphMLReader.inputGraph(graph, inputStream)
    graph
  }

  val graph = convertGraph(readGraph("/home/matt/coding/delme/target/dependencies-compile.graphml"))

  private def render[T](graph: Graph[T]): String = {
    val layeringCalculator = new LayeringCalculator[T]
    val layering = layeringCalculator.assignLayers(graph)
    val layouter = new Layouter[Int](ToStringVertexRenderingStrategy)
    val elements = layouter.layout(LayerOrderingCalculator.reorder(layering))
    Renderer.render(elements)
  }

  render(graph)

}