package com.github.mdr.ascii.graph

import com.github.mdr.ascii.diagram.Box
import com.github.mdr.ascii.diagram.Diagram
import com.github.mdr.ascii.util.Utils.makeMap

object DiagramToGraphConvertor {

  def toGraph(diagram: Diagram): Graph[String] = {
    val boxToVertexMap: Map[Box, String] = makeMap(diagram.childBoxes, _.text)

    val vertices = boxToVertexMap.values.toSet
    val edges =
      for {
        edge ← diagram.allEdges
        vertex1 ← boxToVertexMap.get(edge.box1)
        vertex2 ← boxToVertexMap.get(edge.box2)
      } yield {
        if (edge.hasArrow2)
          vertex1 → vertex2
        else
          vertex2 → vertex1
      }
    Graph(vertices, edges)
  }

}
