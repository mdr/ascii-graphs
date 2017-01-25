package com.github.mdr.ascii.graph

import org.scalacheck._
import org.scalacheck.Gen.Parameters

import scala.util.Random
import com.github.mdr.ascii.layout.cycles.CycleRemover
import org.scalacheck.rng.Seed

object GraphGenerators {

  implicit val graphGen: Gen[Graph[String]] = Gen.choose(0, 9999999).map { seed ⇒ RandomGraph.randomGraph(new Random(scala.util.Random.nextLong)) }

  implicit val arbitraryGraph = Arbitrary(graphGen)

  implicit val shrinkGraph = Shrink { g: Graph[String] ⇒
    // println("Shrink! |v| = " + g.vertices.size + ", |g| = " + g.edges.size)
    (for (edge ← g.edges.toStream) yield g.removeEdge(edge)) append
      (for (v ← g.vertices.toStream) yield g.removeVertex(v))
  }

  private def makeDag[V](g: Graph[V]): Graph[V] = CycleRemover.removeCycles(g).dag

  val dags: Gen[Graph[String]] = graphGen.map(makeDag)

}