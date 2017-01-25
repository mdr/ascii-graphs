package com.github.mdr.ascii

import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.graph.RandomGraph
import com.github.mdr.ascii.layout.coordAssign._
import com.github.mdr.ascii.layout._
import com.github.mdr.ascii.layout.cycles.CycleRemover
import com.github.mdr.ascii.layout.layering.LayerOrderingCalculator
import com.github.mdr.ascii.layout.layering.LayeringCalculator
import com.github.mdr.ascii.util.Utils
import com.github.mdr.ascii.layout.drawing._
import scala.util.Random
import com.github.mdr.ascii.layout.prefs.LayoutPrefsImpl

object RunLayout extends App {

  println(Graph.fromDiagram(""))

  val graph1 = Graph(
    vertices = Set(
      1, 2, 3, 4, 5, 6, 7, 8123, 9, 120
    ),
    edges = List(
      1 → 3,
      1 → 4,
      3 → 8123,
      2 → 5,
      2 → 6,
      3 → 7,
      5 → 9,
      7 → 9
    )
  )

  val V1 = "111111"
  val V2 = "22222222"
  val V3 = "3333333"
  val V4 = "4444444"
  val V5 = "5555555555"
  val V6 = "666666666666"
  val V7 = "777777777777"

  val graph2 = Graph(
    vertices = Set(
      V1, V2, V3, V4, V5, V6, V7
    ),
    edges = List(
      V1 → V2,
      V7 → V1,
      V1 → V3,
      V1 → V4,
      V2 → V5,
      V2 → V6,
      V6 → V1,
      V3 → V7
    )
  )

  val graph3 = Graph.fromDiagram(
    """
                           +---------------+                          
                           |something_2.9.1|                          
                           |  com.example  |                          
                           |    1.0.0.0    |                          
                           +---------------+                          
                                |    |                                
                                |    ---------------                  
                          -------                  |                  
                          |                        |                  
                          v                        |                  
                   +------------+                  |                  
                   |neo4j-cypher|                  |                  
                   | org.neo4j  |                  |                  
                   |  1.8.M04   |                  |                  
                   +------------+                  |                  
                     | | | | |                     |                  
                     | | | | |                     -------------------
                     | | | | --------------------------------------- |
                     | | | --------------------------------------- | |
                     | | ------------------------------          | | |
                     | ----------                     |          | | |
           -----------          |                     |          | | |
           |                    |                     |          | | |
           v                    v                     v          | | |
  +----------------+ +--------------------+ +------------------+ | | |
  |neo4j-graph-algo| |neo4j-graph-matching| |neo4j-lucene-index| | | |
  |   org.neo4j    | |     org.neo4j      | |    org.neo4j     | | | |
  |    1.8.M04     | |      1.8.M04       | |     1.8.M04      | | | |
  +----------------+ +--------------------+ +------------------+ | | |
           |                    |                 |     |        | | |
           ---                  |                 |     |        | | |
             | ------------------                 |     |        | | |
             | | ----------------------------------     |        | | |
             | | | -------------------------------------+--------+-- |
             | | | |               ----------------------        |   |
             | | | |               |          --------------------   |
             | | | |               |          |          -------------
             | | | |               |          |          |            
             v v v v               |          |          |            
           +------------+          |          |          |            
           |neo4j-kernel|          |          |          |            
           | org.neo4j  |          |          |          |            
           |  1.8.M04   |          |          |          |            
           +------------+          |          |          |            
                  |                |          |          |            
                  |                |          |          ----         
                  |                |          ----------    |         
                  |                -----               |    |         
               ----                    |               |    |         
               |                       |               |    |         
               v                       v               v    v         
  +-------------------------+ +-----------------+ +--------------+    
  |  geronimo-jta_1.1_spec  | |   lucene-core   | |scala-library |    
  |org.apache.geronimo.specs| |org.apache.lucene| |org.scala-lang|    
  |          1.1.1          | |      3.5.0      | |    2.9.1     |    
  +-------------------------+ +-----------------+ +--------------+    
"""
  )

  val v1 = """Person
             |────────────
             |name = "Bob"
             |age = 42""".stripMargin
  val v2 = """Person
              |──────────────
              |name = "Alice"
              |age = 35""".stripMargin
  val v3 = """Company
              |────────────────────
              |name = "Thingies Ltd"
              |employees = 123""".stripMargin
  val vertices4 = Set(v1, v2, v3)
  val edges4 = List(v1 → v2, v3 → v1, v3 → v2)

  val graph4 = Graph(vertices4, edges4)

  var seed = new Random().nextInt
  // seed = 426608661 // reverse sweep
  seed = -323464708 // inf loop
  println("Seed = " + seed)
  implicit val random = new Random(seed)
  val graph5 = RandomGraph.randomGraph(random)

  val graph = graph5
  //  println(graph)

  val cycleRemovalResult = CycleRemover.removeCycles(graph)
  val (layering, _) = new LayeringCalculator[String].assignLayers(cycleRemovalResult)
  val reorderedLayering = LayerOrderingCalculator.reorder(layering)
  val layouter = new Layouter(ToStringVertexRenderingStrategy)
  val drawing0 = layouter.layout(reorderedLayering)
  val updatedDrawing1 = KinkRemover.removeKinks(drawing0)
  val updatedDrawing2 = EdgeElevator.elevateEdges(updatedDrawing1)
  val updatedDrawing3 = RedundantRowRemover.removeRedundantRows(updatedDrawing2)
  val rendered = Renderer.render(updatedDrawing3, LayoutPrefsImpl())
  // println(rendered)
  println(GraphLayout.renderGraph(graph))
  if (true)
    for (n ← 1 to 4000) {
      val seed = new Random().nextInt
      println(seed + ": ")
      println(GraphLayout.renderGraph(RandomGraph.randomGraph(new Random(seed))))
    }

}