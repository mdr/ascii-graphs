package com.github.mdr.ascii.layout.layering

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.graph.GraphGenerators._
import com.github.mdr.ascii.layout.cycles.CycleRemover

object AssignLayersSpecification extends Properties("Assign Layers") {

  property("layer assignment respects edge ordering") = forAll { g: Graph[String] ⇒
    val cycleRemovalResult = CycleRemover.removeCycles(g)
    val (layering, realVertices) = new LayeringCalculator[String].assignLayers(cycleRemovalResult)
    val getLayerIndex: Map[Vertex, Int] =
      (for {
        (layer, index) ← layering.layers.zipWithIndex
        v ← layer.vertices
      } yield v → index).toMap
    cycleRemovalResult.dag.edges.forall {
      case (from, to) ⇒
        getLayerIndex(realVertices(from)) < getLayerIndex(realVertices(to))
    }
  }

  property("all vertices accounted for") = forAll { g: Graph[String] ⇒
    val cycleRemovalResult = CycleRemover.removeCycles(g)
    val (layering, realVertices) = new LayeringCalculator[String].assignLayers(cycleRemovalResult)
    val verticesInLayers = layering.layers.flatMap(_.vertices.collect { case v: RealVertex ⇒ v }).toSet
    cycleRemovalResult.dag.vertices.map(realVertices) == verticesInLayers
  }

}