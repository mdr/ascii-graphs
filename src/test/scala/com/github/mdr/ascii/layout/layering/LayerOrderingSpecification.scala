package com.github.mdr.ascii.layout.layering

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.graph.GraphGenerators._
import com.github.mdr.ascii.layout.cycles.CycleRemover

object LayerOrderingSpecification extends Properties("Layer ordering") {

  property("only rearranges vertices in layers") = forAll { g: Graph[String] ⇒
    val cycleRemovalResult = CycleRemover.removeCycles(g)
    val (layering, _) = new LayeringCalculator[String].assignLayers(cycleRemovalResult)
    val newLayering = LayerOrderingCalculator.reorder(layering)
    layering.edges == newLayering.edges &&
      layering.layers.map(_.vertices.toSet) == newLayering.layers.map(_.vertices.toSet)
  }

}
