package com.github.mdr.ascii.diagram.parser

import org.scalatest.{FlatSpec, Matchers}
import com.github.mdr.ascii.diagram.Diagram
import com.github.mdr.ascii.diagram.Box
import com.github.mdr.ascii.util.Utils

import scala.io.Source

class GraphDiagramParserTest extends FlatSpec with Matchers {

  "Parser" should "parse labels" in {

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
       +-+  [7]  +-+          """)

    diagram.allBoxes.map(_.text).toSet should equal("ABCDEF".map(_.toString).toSet)

  }

  it should "parse complex edge paths" in {
    val diagram = Diagram("""
       +-+  +-+  
       |A|  |B|
       +-+  +-+    
        |    |
        -----|---       
             |  |
        |-------|
        |    | 
        -----+----
             v   v
            +-+ +-+ 
            |C| |D|  
            +-+ +-+
    """)
    checkEdges(diagram, "A" → "D", "B" → "C")
  }

  it should "support Unicode box-drawing characters" in {

    val diagram = Diagram("""
      ╭─────╮   
      │  A  │   
      ╰┬──┬─╯   
       │  │     
       │  ╰──╮  
       │     │  
       v     v  
     ╭───╮ ╭───╮
     │ B │ │ C │
     ╰───╯ ╰───╯""")

    diagram.allBoxes.map(text).toSet should equal(Set("A", "B", "C"))
    checkEdges(diagram, "A" → "B", "A" → "C")
  }

  it should "support Unicode box-drawing characters 2" in {
    val diagram = Diagram("""             
                  ╭──╮
      ╭──────╮ ╭─>│ee│
      │feadda├─╯  ╰──╯
      │      ├─╮      
      ╰──────╯ │  ╭──╮
               │  │db│
               ╰─>│fb│
                  ╰──╯
       """)
    checkEdges(diagram, "feadda" → "ee", "feadda" → "db\nfb")
  }

  it should "support Unicode box-drawing characters 3" in {
    val diagram = Diagram("""                
                 ╭──╮
              ╭─>│xx│
              │  ╰──╯ ╭──╮               
     ╭──────╮ │╭─────>│yy│               
     │foobar├─╯│      ╰──╯
     │      ├──╯                 
     ╰──────╯
        
       """)
    checkEdges(diagram, "foobar" → "xx", "foobar" → "yy")
  }

  it should "parse an empty box" in {
    val diagram = Diagram("""                
      ╭─╮
      ╰─╯
    """)
    val List(box) = diagram.allBoxes
    box.text should be("")
    diagram.allEdges should be(Nil)
  }

  it should "find an edge between adjacent connected boxes (Unicode)" in {
    val diagram = Diagram("""                
     ╭─╮╭─╮
     │A├┤B│
     ╰─╯╰─╯
    """)
    diagram.allBoxes.map(text).toSet should equal(Set("A", "B"))
    checkEdges(diagram, "A" → "B")
  }

  it should "not find an edge between adjacent connected boxes (ASCII)" in {
    val diagram = Diagram("""                
     +-++-+
     |A||B|
     +-++-+
    """)
    diagram.allBoxes.map(text).toSet should equal(Set("A", "B"))
    diagram.allEdges should be(Nil)
  }

  it should "handle arrows along edge" in {
    val diagram = Diagram("""                
               ╭─────v               
     ╭──────╮  │   ╭───╮   
     │foobar├──╯   │yyy│              
     ╰──────╯      ╰───╯
       """)
    checkEdges(diagram, "foobar" → "yyy")
  }

  it should "handle misc arrows" in {
    val diagram = Diagram("""                
               ╭──╮                     ╭──────╮             
               │AA├────────────────────>│      │   
            ╭─>│  ├─╮   ╭────────────╮  │      │              
            │  ╰──╯ │   │            │  │      │
   ╭──────╮ │       │   │    ╭───╮   │  │  FF  │
   │      ├─╯       ╰───┼───>│   │   │  │      │
   │  BB  ├─────────────╯╭───┤ D ├──╮│  │      │
   │      │<─────────────╯╭─>│   │<╮││  │      │
   ╰──────╯               │  ╰───╯ │││  ╰──────╯
               ╭──────╮   │        │││          
               │      │   │        │││  ╭──╮    
               │  CC  │   │        ││╰─>│  │    
               │      ├───╯        │╰──>│EE│    
               │      │            ╰────┤  │    
               ╰──────╯                 ╰──╯    
   """)
    checkEdges(
      diagram,
      "AA" → "FF",
      "AA" → "D",
      "BB" → "AA",
      "BB" → "EE",
      "CC" → "D",
      "D" → "BB",
      "D" → "EE",
      "EE" → "D"
    )

  }

  it should "handle bug" in {
    val diagram = Diagram("""                
     ┌───────┐ ┌───┐
     │   P   │ │ H │
     └─┬┬──┬─┘ └─┬─┘
       ││  │     │  
       ││  │ ┌───┘  
       ││  │ │      
       ││  v v      
       ││┌─────┐    
       │││  T  │    
       ││└─┬─┬─┘    
       └┼┐ │ │      
      ┌─┘│ │ │      
      │  v v v      
      │┌───────┐    
      ││   O   │    
      │└────┬──┘    
      │     │       
      └───┐ │       
          │ │       
          v v       
        ┌─────┐     
        │  N  │     
        └─────┘     
        """)
    checkEdges(
      diagram,
      "P" → "T",
      "P" → "O",
      "P" → "N",
      "T" → "O",
      "H" → "T",
      "T" → "O",
      "O" → "N"
    )
  }

  it should "support ASCII edge bend characters" in {
    val diagram = Diagram("""                
               +-+      
         +-+   | |      
    +-+  | |   |D|      
    | |->|H|-->| |      
    | |  | |/->| |      
    |I|  +-+|  +-+      
    | |-----/           
    +-+             
       """)
    checkEdges(diagram, "I" → "H", "H" → "D", "I" → "D")
  }

  it should "support ASCII edge bend characters 2" in {
    val diagram = Diagram("""                
         +-+     +-+
         | |     | |
    +-+  | |---->| |    
    | |  |M|     |W|    
    | |->| |  /->| |    
    | |->| |  |  | |
    | |  +-+  |  +-+
    |A|       |     
    | |<-----\|  +-+
    | |      ||  | |
    | |-----\||  | |
    | |     |\---|D|
    +-+  +-+\--->| |
         | |  |  | |
         |E|--/  +-+
         | |        
         +-+
    """)
    checkEdges(diagram, "A" → "D", "E" → "W", "D" → "A", "A" → "M", "A" → "M", "M" → "W")
  }

  it should "support ASCII edge bend characters 3" in {
    val diagram = Diagram("""                
           +-+        +-+
           | |  +-+   | |
           | |  | |-->| |
      +-+  |J|->| |   | |
      | |  | |  | |   |L|
      | |<-| |<-|K|<--| |
      | |  +-+  | |/->| |
      |F|------>| ||  | |
      | |       | ||  +-+
      | |       +-+|     
      | |----------/  +-+
      +-+             |R|
                      +-+
                         
                      +-+
                      |S|
                      +-+
         """)
    checkEdges(diagram, "F" → "L",
      "J" → "K",
      "L" → "K",
      "K" → "J",
      "F" → "K",
      "J" → "F",
      "K" → "L")
  }

  it should "support ASCII edge bend characters 4" in {
    val diagram = Diagram("""                
         +-----+       
         |  D  |       
         +-----+       
            | +---+ +---+
            | | I | | R |
            | +---+ +---+
            |   |     |  
            |   \-\   |  
            \----\|   |  
          /-----------/  
          |      ||      
          v      vv      
      +-----+ +-----+    
      |  C  | |  H  |    
      +-----+ +-----+    
    """)
    checkEdges(
      diagram,
      "R" → "C",
      "I" → "H",
      "D" → "H"
    )
  }

  it should "Parse double-edged boxes" in {
    val diagram = Diagram("""
      ╔════════╗
      ║nekohtml║
      ╚════════╝        
    """)
    val List(box) = diagram.allBoxes
    box.text should be("nekohtml")

  }

  it should "Parse large diagrams" in {
    val diagram = Diagram(Utils.getResourceAsString("/large1.graph"))
    diagram.allBoxes.size should be(39)
  }

  private def checkEdges(diagram: Diagram, expectedEdges: (String, String)*) {
    val edges = diagram.allEdges.map(e ⇒ text(e.box1) → text(e.box2))
    edges.toSet should equal(expectedEdges.toSet)
    Utils.multisetCompare(edges, expectedEdges.toList) should be(true)
  }

  private def text(box: Box) = box.text.trim

}