package com.github.mdr.ascii.layout

import org.scalatest.{ Matchers, FlatSpec }
import org.scalatest.matchers.ShouldMatchers
import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.layout.RoundTripSpecification._

class RoundTripTest extends FlatSpec with Matchers {

  "Round trip" should ("not overwrite an arrow") in {
    checkRoundTrip(Graph.fromDiagram("""
        ╭────╮   ╭──────╮    
        │aaff│   │afcfad│    
        │ db │   │ ced  │    
        ╰───┬╯   ╰──┬─┬─╯    
            │       │ │      
            │       │ ╰─────╮
            │       ╰──╮    │
            │          │    │
            v          v    │
      ╭───────────╮ ╭─────╮ │
      │ddabcfabcbe│ │eeed │ │
      │   ccfda   │ │ cb  │ │
      ╰────┬──────╯ ╰──┬──╯ │
           │           │ ^  │  <-- was a bug where the edge segment below would be elevated into the arrow
           │   ╭───────┼─┼──╯
           │   │       │ │   
           v   v       │ │   
      ╭────────────╮   v │   
      │     d      │ ╭───┴─╮ 
      │     d      │ │  c  │ 
      │baecababeedc│ ╰─────╯ 
      ╰────────────╯         
  """), unicodeLayoutPrefs) should be(true)
  }

}