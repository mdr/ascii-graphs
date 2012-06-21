package com.github.mdr.ascii.layout

import com.github.mdr.ascii.util.Utils

object LayerOrderingCalculator {

  def reorder(layering: Layering): Layering =
    layering.copy(layers =
      for ((previousLayerOpt, currentLayer) ← Utils.withPrevious(layering.layers))
        yield previousLayerOpt match {
        case Some(previousLayer) ⇒ reorder(previousLayer, currentLayer, layering.edges)
        case None                ⇒ currentLayer
      })

  def reorder(layer1: Layer, layer2: Layer, edges: List[Edge]): Layer = {
    def inVertices(vertex: Vertex): List[Vertex] = edges.collect { case Edge(v1, `vertex`) ⇒ v1 }
    def barycenter(vertex: Vertex): Double = {
      val in = inVertices(vertex)
      in.map(v ⇒ layer1.vertices.indexOf(v)).sum.toDouble / in.size
    }
    layer2.copy(vertices = layer2.vertices.sortBy(barycenter))
  }

}