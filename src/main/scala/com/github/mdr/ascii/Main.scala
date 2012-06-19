package com.github.mdr.ascii

import com.github.mdr.ascii.layout._

object Main extends App {

  val r = new Renderer
  val v1: Vertex = new RealVertex("1")
  val v2: Vertex = new RealVertex("2")
  val v3: Vertex = new RealVertex("3")
  val v4: Vertex = new RealVertex("4")
  val v5: Vertex = new RealVertex("5")
  val v6: Vertex = new RealVertex("6")
  val v7: Vertex = new RealVertex("7")
  val v8: Vertex = new RealVertex("8")
  val result = // colliding edges
    Renderer.render(Layouter.layout(
      List(v1, v2, v3),
      List(v4, v5, v6, v7, v8),
      for (from <- List(v1, v2, v3); to <- List(v4, v5, v6, v7, v8)) yield from -> to))
  //        List(v1 -> v5, v2 -> v8, v3 -> v7, v3 -> v4, v1 -> v4, v1 -> v4)))
  //  val result =
  //    Renderer.render(Layouter.layout(
  //      List(v1, v2, v3),
  //      List(v4, v5, v6, v7, v8),
  ////      List(v3 -> v4, v2 -> v4)))
  ////        List(v1 -> v8, v2 -> v8)))
  //        List(v3 -> v8, v1 -> v4, v2 -> v5, v2 -> v6, v1 -> v7, v3 -> v4)))

  println(result)
  System.exit(1)

  //  val el1 = new VertexDrawingElement(Region(Point(10, 2), Point(20, 20)), List("foo"))
  //  val el2 = new VertexDrawingElement(Region(Point(30, 10), Point(40, 40)), List("foo123", "bar456"))
  //  val el3 = new EdgeDrawingElement(List(Point(32, 30), Point(32, 35), Point(38, 35), Point(38, 27)), true, true)

  //  val result = r.render(List(el1, el2, el3))
  //  println(result)
  //  System.exit(1)

  val diagram1 = Diagram("""
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

  val diagram2 = Diagram(""" 
    +-----------+
    |  +--+     |     +-+   +---------+
----|  |d | asdf|     | |   |The quick|
|   |  +--+     |---->| |   |brown fox|<--
|   +-----------+     +-+   +---------+  |
|       |                       ^        |
|       |           adsf        |        |
--------+-----------------------+---------
        |                       |
        -------------------------
  """)

  {

    val diagram = Diagram("""
    +----------+          +------------+
    |Person    |          |Person      |
    |==========| [likes]  |============|
    |Name: Bob |--------->|Name: Alice |
    | Age: 25  |          | Age: 26    |
    +----------+          +------------+
         ^                       |
         |                       |[knows]
         |                       v
         |[employs]       +------------+
         |                |Person      |
         |                |============|
         -----------------|Name: Carl  |
                          | Age: 34    |
                          +------------+  """)

  }

  {
    val diagram = diagram1

    for (box ← diagram.allBoxes)
      println(box)

    for {
      box ← diagram.allBoxes.find(_.text == "A")
      (edge, otherBox) ← box.connections()
      label ← edge.label
    } println(box + " ==> " + label + " ==> " + otherBox)
  }
  //  for (box ← diagram.allBoxes)
  //    println(box)
  //  for (edge ← diagram.allEdges)
  //    println(edge)

}