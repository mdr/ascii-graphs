package com.github.mdr.ascii

import com.github.mdr.ascii.diagram.Diagram

object Main {

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