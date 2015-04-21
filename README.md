An ASCII-art diagram library for graphs. It supports both parsing existing diagrams and rendering graphs out as an ASCII diagram.

You can use it via sbt:

    libraryDependencies += "com.github.jlmauduy" %% "ascii-graphs" % "0.0.3"

# Graph layout

    import com.github.mdr.ascii.layout._

    val graph = Graph(
      vertices = List(
        "V1", "V2", "V3", "V4", "V5", "V6", "V7"),
      edges = List(
        "V1" -> "V2",
        "V7" -> "V1",
        "V1" -> "V3",
        "V1" -> "V4",
        "V2" -> "V5",
        "V2" -> "V6"))

    val ascii = Layouter.renderGraph(graph)

    println(ascii)

This would produce:

             +---+         
             |V7 |         
             +---+         
               |           
               v           
           +-------+       
           |  V1   |       
           +-------+       
             |  ||         
         -----  |--------  
         |      ---     |  
         v        |     |  
      +-----+     |     |  
      | V2  |     |     |  
      +-----+     |     |  
        | |       |     |  
      --- ---     |     |  
      |     |     |     |  
      v     v     v     v  
    +---+ +---+ +---+ +---+
    |V5 | |V6 | |V4 | |V3 |
    +---+ +---+ +---+ +---+

Layout is Sugiyama-style layered graph drawing, and supports multi-edges, cycles, and vertex labels, but not self-loops or edge labels.

## Other ASCII layout libraries

* Vijual (Clojure): http://lisperati.com/vijual/
* Graph::Easy (Perl): http://bloodgate.com/perl/graph/manual/

# Graph parser

The graph parser is intended for constructing test DSLs, particularly for data which would be much more comprehensible in ASCII art
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
  
