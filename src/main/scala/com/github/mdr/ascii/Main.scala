package com.github.mdr.ascii

object Main extends App {

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

  val diagram = diagram1

  for (box ← diagram.allBoxes)
    println(box)
  for (edge ← diagram.allEdges)
    println(edge)

}