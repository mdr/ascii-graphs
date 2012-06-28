package com.github.mdr.ascii.layout

import com.github.mdr.ascii.Dimension

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
              +--------+              
              |22222222|              
              +--------+              
                | ^ |                 
                | | -----------       
                | -------     |       
            -----       |     |       
            |           |     |       
            v           |     |       
     +------------+     |     |       
     |666666666666|     |     |       
     +------------+     |     |       
            |           |     |       
            |  ----------     |       
            |  |           ----       
            |  |           |          
            v  |           |          
         +-------+         |          
         |111111 |         |          
         +-------+         |          
           | ^ |           |          
           | | --------    |          
           | -----    |    |          
        ----     |    |    |          
        |        |    |    |          
        v        |    |    |          
    +-------+    |    |    |          
    |3333333|    |    |    |          
    +-------+    |    |    |          
        |        |    |    |          
        |        |    |    ------     
     ----        |    |         |     
     |   ---------    |         |     
     |   |          ---         |     
     |   |          |           |     
     v   |          v           v     
 +------------+ +-------+ +----------+
 |777777777777| |4444444| |5555555555|
 +------------+ +-------+ +----------+
""")
  val graph = graph3

  println(graph)

  val (newGraph, reversedEdges) = new CycleRemover[String].removeCycles(graph)
  println("reversedEdges: " + reversedEdges)
  val layeringCalculator = new LayeringCalculator[String]
  val layering = layeringCalculator.assignLayers(newGraph, reversedEdges.toSet)
  //  println(layering)

  val layouter = new Layouter[Int](ToStringVertexRenderingStrategy)
  val elements = layouter.layout(LayerOrderingCalculator.reorder(layering))
  //  elements.foreach(println)
  val result = Renderer.render(elements)

  println(result)

}