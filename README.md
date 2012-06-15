An ASCII-art diagram parser for graphs.

This is intended for constructing test DSLs, particularly for data which would be much more comprehensible in ASCII art
than constructed through regular programming language expressions. For example, directed graphs, trees, 2D games, 
object graphs, and so on.

Typical usage is to parse the diagram into `Diagram`/`Box`/`Edge` objects, and then convert those objects into 
whatever your specific test data happens to be.

## Syntax

### Boxes


A `Box` is drawn as follows:

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

### Edges

An `Edge` connects two boxes:

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

You can connect a child box to its parent:

    +--------------+
    |              |
    |   +-----+    |
    |   |     |    |
    |   |     |----|
    |   +-----+    |
    |              |
    +--------------+   
    
Edges can cross using a `+`:

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

### Labels

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
    
The label's `[` or `]` bracket must be adjacent (horizontally or vertically) to part of the edge.

## Usage

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
  
    // Print all the vertices neighbouring A along with the edge weight:
    for {
      box ← diagram.allBoxes.find(_.text == "A")
      (edge, otherBox) ← box.connections()
      label ← edge.label
    } println(box + " ==> " + label + " ==> " + otherBox)
  
