package com.github.mdr.ascii.layout.layering

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.graph.GraphUtils
import com.github.mdr.ascii.util.Utils

class LongestDistanceToSinkTest extends FlatSpec with ShouldMatchers {

  distancesToSinks("""
         +-+   +-+   +-+ 
         |A|-->|B|-->|C| 
         +-+   +-+   +-+ """) shouldBe
    ("A" -> 2, "B" -> 1, "C" -> 0)

  distancesToSinks("""
         +-+   +-+   +-+ 
         |A|   |B|-->|C| 
         +-+   +-+   +-+ 
          |           ^  
          |           |  
          -------------  """) shouldBe
    ("A" -> 1, "B" -> 1, "C" -> 0)

  distancesToSinks("""
         +-+   +-+   +-+ 
         |A|-->|B|-->|C| 
         +-+   +-+   +-+ 
          |           ^  
          |           |  
          -------------  """) shouldBe
    ("A" -> 2, "B" -> 1, "C" -> 0)

  distancesToSinks("""
          +-+ 
          |B| 
          +-+ 
           """) shouldBe
    ("B" -> 0)

  distancesToSinks("") shouldBe ()

  def distancesToSinks(diagram: String) = new {
    val graph = Graph.fromDiagram(diagram)
    val layeringCalculator = new LayeringCalculator[String]
    def shouldBe(expectedDistances: (String, Int)*) {
      "Distances to sinks" should ("be correct for " + ">>>\n" + graph + "\n<<<") in {
        val actualDistances = layeringCalculator.calculateLongestDistances(graph)
        actualDistances should be(expectedDistances.toMap)
      }
    }
  }

}