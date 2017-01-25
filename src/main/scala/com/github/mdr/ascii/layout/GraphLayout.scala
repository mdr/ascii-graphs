package com.github.mdr.ascii.layout

import com.github.mdr.ascii.graph.Graph
import com.github.mdr.ascii.layout.coordAssign._
import com.github.mdr.ascii.layout.cycles.CycleRemover
import com.github.mdr.ascii.layout.drawing._
import com.github.mdr.ascii.layout.layering._
import com.github.mdr.ascii.layout.prefs.LayoutPrefsImpl
import com.github.mdr.ascii.layout.prefs.LayoutPrefs

object GraphLayout {

  /**
   * Layout a graph as a String using toString() on the vertices
   */
  def renderGraph[V](graph: Graph[V]): String =
    renderGraph(graph, ToStringVertexRenderingStrategy, LayoutPrefsImpl())

  def renderGraph[V](
    graph: Graph[V],
    vertexRenderingStrategy: VertexRenderingStrategy[V] = ToStringVertexRenderingStrategy,
    layoutPrefs: LayoutPrefs = LayoutPrefsImpl()
  ): String = {
    val cycleRemovalResult = CycleRemover.removeCycles(graph)
    val (layering, _) = new LayeringCalculator[V].assignLayers(cycleRemovalResult)
    val reorderedLayering = LayerOrderingCalculator.reorder(layering)
    val layouter = new Layouter(ToStringVertexRenderingStrategy, layoutPrefs.vertical)
    var drawing = layouter.layout(reorderedLayering)

    if (layoutPrefs.removeKinks)
      drawing = KinkRemover.removeKinks(drawing)
    if (layoutPrefs.elevateEdges)
      drawing = EdgeElevator.elevateEdges(drawing)
    if (layoutPrefs.compactify)
      drawing = RedundantRowRemover.removeRedundantRows(drawing)

    if (!layoutPrefs.vertical)
      drawing = drawing.transpose
    Renderer.render(drawing, layoutPrefs)
  }

}