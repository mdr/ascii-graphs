package com.github.mdr.ascii

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import com.github.mdr.ascii.diagram.Diagram

class GraphDiagramParserTest extends FlatSpec with ShouldMatchers {

  "Parser" should "work" in {

    val diagram = Diagram("""
             +-+             
    ---------|E|----------   
    |        +-+         |   
 [9]|                 [6]|   
    |                    |   
   +-+  [2]  +-+  [11]  +-+  
   |F|-------|C|--------|D|  
   +-+       +-+        +-+  
    |         |          |   
[14]|     [10]|      [15]|   
    |         |          |   
   +-+       +-+         |   
   |A|-------|B|----------   
   +-+  [7]  +-+          """)

    diagram.allBoxes.map(_.text).toSet should equal("ABCDEF".map(_.toString).toSet)

  }

}