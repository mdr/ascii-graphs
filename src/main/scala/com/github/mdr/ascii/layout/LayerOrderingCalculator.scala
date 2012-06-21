package com.github.mdr.ascii.layout

import com.github.mdr.ascii.util.Utils

object LayerOrderingCalculator {

  def reorder(vertexLayers: List[List[Vertex]], edges: List[(Vertex, Vertex)]): List[List[Vertex]] = {
    for ((previousLayerOpt, currentLayer) ← Utils.withPrevious(vertexLayers))
      yield previousLayerOpt match {
      case Some(previousLayer) ⇒ reorder(previousLayer, currentLayer, edges)
      case None                ⇒ currentLayer
    }
  }

  def reorder(vertices1: List[Vertex], vertices2: List[Vertex], edges: List[(Vertex, Vertex)]): List[Vertex] = {
    def inVertices(vertex: Vertex): List[Vertex] = edges.collect { case (v1, `vertex`) ⇒ v1 }
    def barycenter(vertex: Vertex): Double = {
      val in = inVertices(vertex)
      in.map(v ⇒ vertices1.indexOf(v)).sum.toDouble / in.size
    }
    vertices2.sortBy(barycenter)
  }

}