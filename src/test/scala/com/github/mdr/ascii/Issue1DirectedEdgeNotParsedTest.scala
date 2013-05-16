package com.github.mdr.ascii

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.diagram.Diagram

class Issue1DirectedEdgeNotParsedTest extends FlatSpec with ShouldMatchers {

  "Parser" should "detect directed edge" in {

    val diagram = Diagram("""
      +-------+         
      | op1   |         
      +-------+         
        |               
        |               
        ----            
           |            
           v            
        +-----+         
        |gbk1 |         
        +-----+         """)

    {
      val boxes = diagram.allBoxes
      val Some(op1) = boxes.find(_.text.contains("op1"))
      val Some(gbk1) = boxes.find(_.text.contains("gbk1"))
      val List(edge) = op1.edges
      edge.hasArrow1 should be(false)
      edge.hasArrow2 should be(true)
    }

    {
      val graph = Graph.fromDiagram(diagram)
      val Some(op1) = graph.vertices.find(_ contains ("op1"))
      val Some(gbk1) = graph.vertices.find(_ contains ("gbk1"))
      val List(edge) = graph.edges
      edge should be((op1, gbk1))
    }

  }

}