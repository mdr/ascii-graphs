package com.github.mdr.ascii

import com.github.mdr.ascii.layout.CycleRemover
import com.github.mdr.ascii.layout.Graph
import com.github.mdr.ascii.layout.LayerOrderingCalculator
import com.github.mdr.ascii.layout.LayeringCalculator
import com.github.mdr.ascii.layout.Layouter
import com.github.mdr.ascii.layout.Renderer
import com.github.mdr.ascii.layout.ToStringVertexRenderingStrategy
import com.github.mdr.ascii.layout.KinkRemover
import com.github.mdr.ascii.layout.Compactifier

object RunLayout extends Application {

  val graph1 = Graph(
    vertices = List(
      1, 2, 3, 4, 5, 6, 7, 8123, 9, 120),
    edges = List(
      1 -> 3,
      1 -> 4,
      3 -> 8123,
      2 -> 5,
      2 -> 6,
      3 -> 7,
      5 -> 9,
      7 -> 9))

  val V1 = "111111"
  val V2 = "22222222"
  val V3 = "3333333"
  val V4 = "4444444"
  val V5 = "5555555555"
  val V6 = "666666666666"
  val V7 = "777777777777"

  val graph2 = Graph(
    vertices = List(
      V1, V2, V3, V4, V5, V6, V7),
    edges = List(
      V1 -> V2,
      V7 -> V1,
      V1 -> V3,
      V1 -> V4,
      V2 -> V5,
      V2 -> V6,
      V6 -> V1,
      V3 -> V7))

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
""")
  val graph = graph3

  //  println(graph)

  val (newGraph, reversedEdges) = new CycleRemover[String].removeCycles(graph)
  //  println("reversedEdges: " + reversedEdges)
  val layeringCalculator = new LayeringCalculator[String]
  val layering = layeringCalculator.assignLayers(newGraph, reversedEdges.toSet)
  //  println(layering)

  val layouter = new Layouter[Int](ToStringVertexRenderingStrategy)
  val drawing0 = layouter.layout(LayerOrderingCalculator.reorder(layering))
  println(Renderer.render(drawing0))
  //  elements.foreach(println)
  val updatedDrawing1 = KinkRemover.removeKinks(drawing0)
  println(Renderer.render(updatedDrawing1))
  val updatedDrawing2 = Compactifier.compactify(updatedDrawing1)
  println(Renderer.render(updatedDrawing2))
  //  val updatedDrawing3 = KinkRemover.removeKinks(updatedDrawing2)
  //  println(Renderer.render(updatedDrawing3))
  //  val updatedDrawing4 = Compactifier.compactify(updatedDrawing3)
  //  println(Renderer.render(updatedDrawing4))
  //  val updatedDrawing5 = KinkRemover.removeKinks(updatedDrawing4)
  //  println(Renderer.render(updatedDrawing5))

}