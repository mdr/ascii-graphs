package com.github.mdr.ascii.layout

import com.github.mdr.ascii.parser.Translatable
import com.github.mdr.ascii.parser.Point
import com.github.mdr.ascii.parser.Region
import com.github.mdr.ascii.util.Utils
import com.github.mdr.ascii.layout.layering.Edge

/**
 * Info for rendering a vertex.
 *
 * @param region - area allocated to the vertex box.
 * @param inPorts - map of incoming edges to the points they connect to on the vertex box.
 * @param outPorts - map of outgoing edges to the points they connect to on the vertex box.
 */
case class VertexInfo(boxRegion: Region, inPorts: Map[Edge, Point], outPorts: Map[Edge, Point]) extends Translatable[VertexInfo] {

  def contentRegion: Region = boxRegion.copy(topLeft = boxRegion.topLeft.down.right, bottomRight = boxRegion.bottomRight.up.left)

  def translate(down: Int = 0, right: Int = 0): VertexInfo = {
    val newRegion = boxRegion.translate(down, right)
    VertexInfo(
      boxRegion.translate(down, right),
      Utils.transformValues(inPorts)(_.translate(down, right)),
      Utils.transformValues(outPorts)(_.translate(down, right)))
  }

  def setLeft(column: Int): VertexInfo = translate(right = column - boxRegion.leftColumn)

}
