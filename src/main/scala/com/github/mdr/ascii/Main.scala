package com.github.mdr.ascii

import com.github.mdr.ascii.layout._

object Main extends App {

  val r = new Renderer

  val el1 = new VertexDrawingElement(Region(Point(10, 2), Point(20, 20)), List("foo"))
  val el2 = new VertexDrawingElement(Region(Point(30, 10), Point(40, 40)), List("foo123", "bar456"))
  val el3 = new EdgeDrawingElement(List(Point(32, 30), Point(32, 35), Point(38, 35), Point(38, 27)), true, true)

  val result = r.render(List(el1, el2, el3))
  println(result)
  System.exit(1)

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