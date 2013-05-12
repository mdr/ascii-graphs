package com.github.mdr.ascii.layout.layering

/**
 * Reorder the vertices in each layer in an attempt to minimise crossings.
 */
object LayerOrderingCalculator {

  def reorder(layering: Layering): Layering = {
    var previousLayerOpt: Option[Layer] = None
    val newLayers = layering.layers.map { currentLayer ⇒
      val updatedLayer = previousLayerOpt match {
        case Some(previousLayer) ⇒ reorder(previousLayer, currentLayer, layering.edges)
        case None                ⇒ currentLayer
      }
      previousLayerOpt = Some(updatedLayer)
      updatedLayer
    }
    layering.copy(layers = newLayers)
  }

  private def reorder(layer1: Layer, layer2: Layer, edges: List[Edge]): Layer = {
    def barycenter(vertex: Vertex): Double = {
      val inVertices = edges.collect { case Edge(v1, `vertex`) ⇒ v1 }
      inVertices.map(v ⇒ layer1.vertices.indexOf(v)).sum.toDouble / inVertices.size
    }
    val reorderedVertices = layer2.vertices.sortBy(barycenter)
    layer2.copy(vertices = reorderedVertices)
  }

}