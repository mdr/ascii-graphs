package com.github.mdr.ascii.layout

import org.scalacheck.Prop._
import org.scalacheck.Properties
import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.graph.GraphGenerators._
import com.github.mdr.ascii.layout.coordAssign.ToStringVertexRenderingStrategy
import com.github.mdr.ascii.layout.prefs.LayoutPrefsImpl
import com.github.mdr.ascii.layout.prefs.LayoutPrefs

object RoundTripSpecification extends Properties("RoundTrip") {

  property("unicode round trip") = forAll { g: Graph[String] ⇒
    val layoutPrefs = LayoutPrefsImpl(unicode = true, removeKinks = true, compactify = true, vertical = true)
    checkRoundTrip(g, layoutPrefs)
  }

  property("ascii round trip") = forAll { g: Graph[String] ⇒
    val layoutPrefs = LayoutPrefsImpl(unicode = false, removeKinks = true, compactify = true, vertical = true, explicitAsciiBends = true)
    checkRoundTrip(g, layoutPrefs)
  }

  private def checkRoundTrip(g: Graph[String], layoutPrefs: LayoutPrefs): Boolean = {
    val rendered = GraphLayout.renderGraph(g, layoutPrefs = layoutPrefs)
    val graphAgain = removeWhitespace(Graph.fromDiagram(rendered))
    val originalGraph = removeWhitespace(g)
    graphAgain == originalGraph
  }

  private def removeWhitespace(g: Graph[String]): Graph[String] = g.map(_.filterNot(_.isWhitespace))

}
