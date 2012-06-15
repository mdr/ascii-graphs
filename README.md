An ASCII diagram parser for graphs.

Boxes
-----

A Box is drawn as follows:

    +------+
    |      |
    |      |
    |      |
    +------+
    
It can contain text:

    +---------------+
    |The quick brown|
    |fox jumps over |
    |the lazy dog.  |
    +---------------+
    
Or other boxes:

    +-----+
    |+---+|
    ||+-+||
    ||| |||
    ||+-+||
    |+---+|
    +-----+

Edges
-----

Edges connect boxes:

    +-----+
    |     |
    |     |---------
    |     |        |
    +-----+        |
       |           |
       |           |
    +-----+     +-----+
    |     |     |     |
    |     |-----|     |
    |     |     |     |
    +-----+     +-----+

You can connect a child box to its parent:

    +--------------+
    |              |
    |   +-----+    |
    |   |     |    |
    |   |     |----|
    |   +-----+    |
    |              |
    +--------------+   

Edges can have an arrow at either or both ends:

    +-----+
    |     |
    |     |---------
    |     |        |
    +-----+        |
       ^           |
       |           v
    +-----+     +-----+
    |     |     |     |
    |     |<--->|     |
    |     |     |     |
    +-----+     +-----+
    
Edges can cross at a `+`:

             +-----+
             |     |
             |     |
             |     |
             +-----+
                |
    +-----+     |     +-----+
    |     |     |     |     |
    |     |-----+---->|     |
    |     |     |     |     |
    +-----+     |     +-----+
                v
             +-----+
             |     |
             |     |
             |     |
             +-----+    

Labels
------

Edges can have an associated label:

    +-----+
    |     |
    |     |
    |     |
    +-----+
       |
       |[label]
       |
    +-----+         +-----+
    |     | [label] |     |
    |     |---------|     |
    |     |         |     |
    +-----+         +-----+
    
The `[` or `]` bracket must be adjacent (horizontally or vertically) to part of the edge.

Usage
-----

    import com.github.mdr.ascii._
    
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
       +-+  [7]  +-+      """)
    
    for (box ← diagram.allBoxes)
      println(box)
  
    for {
      box ← diagram.allBoxes.find(_.text == "A")
      edge ← box.edges
      label ← edge.label
    } println(label)
  
