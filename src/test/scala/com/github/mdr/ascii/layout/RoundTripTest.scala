package com.github.mdr.ascii.layout

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.layout.RoundTripSpecification._

class RoundTripTest extends FlatSpec with ShouldMatchers {

  "Round trip" should ("work") in {
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
           │           │ ^  │
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