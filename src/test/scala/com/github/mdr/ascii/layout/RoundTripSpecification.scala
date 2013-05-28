package com.github.mdr.ascii.layout

import org.scalacheck.Prop._
import org.scalacheck.Properties

import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.graph.GraphGenerators._
import com.github.mdr.ascii.layout.coordAssign.ToStringVertexRenderingStrategy

object RoundTripSpecification extends Properties("RoundTrip") {

  property("unicode round trip") = forAll { g: Graph[String] ⇒
    val rendered = GraphLayout.renderGraph(g, unicode = true, removeKinks = true, compactify = true, vertical = true)
    val graphAgain = removeWhitespace(Graph.fromDiagram(rendered))
    val originalGraph = removeWhitespace(g)
    graphAgain == originalGraph
  }

  property("ascii round trip") = forAll { g: Graph[String] ⇒
    val rendered = GraphLayout.renderGraph(g, unicode = false, removeKinks = true, compactify = true, vertical = true)
    val graphAgain = removeWhitespace(Graph.fromDiagram(rendered))
    val originalGraph = removeWhitespace(g)
    graphAgain == originalGraph
  }

  private def removeWhitespace(g: Graph[String]): Graph[String] = g.map(_.filterNot(_.isWhitespace))

}
