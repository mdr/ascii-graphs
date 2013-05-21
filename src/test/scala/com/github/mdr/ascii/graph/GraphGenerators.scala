package com.github.mdr.ascii.graph

import scala.util.Random.javaRandomToRandom

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.Params
import org.scalacheck.Shrink

import com.github.mdr.ascii.layout.cycles.CycleRemover

object GraphGenerators {

  implicit val graphGen: Gen[Graph[String]] = Gen { p: Params ⇒ Some(RandomGraph.randomGraph(p.rng)) }

  implicit val arbitraryGraph = Arbitrary(graphGen)

  implicit val shrinkGraph = Shrink { g: Graph[String] ⇒
    // println("Shrink! |v| = " + g.vertices.size + ", |g| = " + g.edges.size)
    (for (edge ← g.edges.toStream) yield g.removeEdge(edge)) append
      (for (v ← g.vertices.toStream) yield g.removeVertex(v))
  }

  private def makeDag[V](g: Graph[V]): Graph[V] = CycleRemover.removeCycles(g).dag

  val dags: Gen[Graph[String]] = graphGen.map(makeDag)

}