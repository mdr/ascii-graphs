package com.github.mdr.ascii.layout.layering

class CrossingCalculator(layer1: Layer, layer2: Layer, edges: List[Edge]) {

  /**
   * @param u, v, two vertices in layer2 such that u is v or u is ordered before v in the layer
   */
  def crossingNumber(u: Vertex, v: Vertex): Int =
    if (u == v)
      0
    else {
      var count = 0
      for {
        Edge(w, u2) ← edges if u2 == u
        Edge(z, v2) ← edges if v2 == v
        if layer1.positionOf(z) < layer1.positionOf(w)
      } count += 1
      count
    }

  /**
   * The number of crossings between two layers
   */
  def numberOfCrossings: Int =
    (for {
      u ← layer2.vertices
      v ← layer2.vertices
      if layer2.positionOf(u) < layer2.positionOf(v)
    } yield crossingNumber(u, v)).sum

}