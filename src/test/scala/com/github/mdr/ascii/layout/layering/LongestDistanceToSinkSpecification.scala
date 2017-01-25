package com.github.mdr.ascii.layout.layering

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.graph.GraphGenerators._

object LongestDistanceToSinkSpecification extends Properties("LongestDistanceToSink") {

  property("longest distance to sink") = forAll(dags) { g: Graph[String] ⇒
    val distances = LongestDistancesToSinkCalculator.longestDistancesToSink(g)
    g.vertices.forall { v ⇒
      distances(v) == paths(g, v).map(_.length).max
    }
  }

  def paths[V](graph: Graph[V], v: V): List[List[V]] = {
    graph.outVertices(v) match {
      case Nil ⇒ List(List[V]())
      case vs  ⇒ vs.flatMap(v2 ⇒ paths(graph, v2).map(v2 :: _))
    }
  }

}