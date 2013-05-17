package com.github.mdr.ascii.layout.layering

/**
 * Reorder the vertices in each layer in an attempt to minimise crossings.
 */
object LayerOrderingCalculator {

  // 426608661 -- reverse sweep would help
  /**
   * Sweep through the layers from top to bottom
   */
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

  /**
   * Reorder the vertices in layer2 to minimise crossings
   */
  private def reorder(layer1: Layer, layer2: Layer, edges: List[Edge]): Layer = {
    def barycenter(vertex: Vertex): Double = {
      val inVertices = edges.collect { case Edge(v1, `vertex`) ⇒ v1 }
      average(inVertices)(v ⇒ layer1.positionOf(v).toDouble)
    }
    layer2.copy(vertices = layer2.vertices.sortBy(barycenter))
  }

  private def average[T](items: Iterable[T])(f: T ⇒ Double): Double =
    items.map(f).sum / items.size

}
