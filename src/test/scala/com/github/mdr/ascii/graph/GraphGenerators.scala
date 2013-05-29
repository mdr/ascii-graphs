package com.github.mdr.ascii.graph

import org.scalacheck._
import org.scalacheck.Gen.Params
import scala.util.Random
import com.github.mdr.ascii.layout.cycles.CycleRemover

object GraphGenerators {

  implicit val graphGen: Gen[Graph[String]] = Gen { p: Params ⇒ Some(RandomGraph.randomGraph(new Random(p.rng))) }

  implicit val arbitraryGraph = Arbitrary(graphGen)

  implicit val shrinkGraph = Shrink { g: Graph[String] ⇒
    // println("Shrink! |v| = " + g.vertices.size + ", |g| = " + g.edges.size)
    (for (edge ← g.edges.toStream) yield g.removeEdge(edge)) append
      (for (v ← g.vertices.toStream) yield g.removeVertex(v))
  }

  private def makeDag[V](g: Graph[V]): Graph[V] = CycleRemover.removeCycles(g).dag

  val dags: Gen[Graph[String]] = graphGen.map(makeDag)

}