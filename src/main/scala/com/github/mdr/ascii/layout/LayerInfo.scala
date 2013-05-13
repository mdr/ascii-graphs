package com.github.mdr.ascii.layout

import com.github.mdr.ascii.parser.Translatable
import com.github.mdr.ascii.layout.layering.RealVertex
import com.github.mdr.ascii.layout.layering.Vertex
import com.github.mdr.ascii.util.Utils
import com.github.mdr.ascii.util.Utils._

/**
 * VertexInfo's for all the vertices in a single layer.
 */
case class LayerInfo(vertexInfos: Map[Vertex, VertexInfo]) extends Translatable[LayerInfo] {

  def vertexInfo(v: Vertex): Option[VertexInfo] = vertexInfos.get(v)

  def isEmpty = vertexInfos.isEmpty

  def maxRow = vertexInfos.values.map(_.boxRegion.bottomRow).fold(0)(_ max _)

  def maxColumn = vertexInfos.values.map(_.boxRegion.rightColumn).fold(0)(_ max _)

  def translate(down: Int = 0, right: Int = 0) =
    copy(vertexInfos = transformValues(vertexInfos)(_.translate(down, right)))

  def realVertexInfos: List[(RealVertex, VertexInfo)] = vertexInfos.toList.collect {
    case (vertex: RealVertex, info) ⇒ (vertex, info)
  }

}