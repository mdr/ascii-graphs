package com.github.mdr.ascii.layout

import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.layout.coordAssign._
import com.github.mdr.ascii.layout.cycles.CycleRemover
import com.github.mdr.ascii.layout.drawing._
import com.github.mdr.ascii.layout.layering._
import com.github.mdr.ascii.layout.prefs.LayoutPrefsImpl

object GraphLayout {

  /**
   * Layout a graph as a String using toString() on the vertices
   */
  def renderGraph[V](graph: Graph[V]): String =
    renderGraph(graph, ToStringVertexRenderingStrategy)

  def renderGraph[V](
    graph: Graph[V],
    vertexRenderingStrategy: VertexRenderingStrategy[V] = ToStringVertexRenderingStrategy,
    removeKinks: Boolean = true,
    compactify: Boolean = true,
    unicode: Boolean = false,
    vertical: Boolean = true): String = {
    val cycleRemovalResult = CycleRemover.removeCycles(graph)
    val (layering, _) = new LayeringCalculator[V].assignLayers(cycleRemovalResult)
    val reorderedLayering = LayerOrderingCalculator.reorder(layering)
    val layouter = new Layouter(ToStringVertexRenderingStrategy, vertical)
    var drawing = layouter.layout(reorderedLayering)
    if (removeKinks)
      drawing = KinkRemover.removeKinks(drawing)
    if (compactify)
      drawing = Compactifier.compactify(drawing)
    if (!vertical)
      drawing = drawing.transpose
    Renderer.render(drawing, LayoutPrefsImpl(unicode = unicode))
  }

}