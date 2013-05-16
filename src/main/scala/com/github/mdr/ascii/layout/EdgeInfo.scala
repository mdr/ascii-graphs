package com.github.mdr.ascii.layout

import com.github.mdr.ascii.layout.layering.Vertex
import com.github.mdr.ascii.common.Point
import com.github.mdr.ascii.util.Utils._

/**
 * Information about edges that pass between two adjacent layers.
 */
case class EdgeInfo(startVertex: Vertex, finishVertex: Vertex, startPort: Point, finishPort: Point, reversed: Boolean) {

  def inRank: Int = signum(startPort.column - finishPort.column) * finishPort.column

  def requiresBend = startPort.column != finishPort.column

}
