package com.github.mdr.ascii.layout.coordAssign

import com.github.mdr.ascii.layout.layering.Vertex
import com.github.mdr.ascii.common.Point
import com.github.mdr.ascii.layout.layering.Edge

/**
 * Information about edges that pass between two adjacent layers.
 *
 * @param finishPort -- column is correct, but not row
 */
case class EdgeInfo(startVertex: Vertex, finishVertex: Vertex, startPort: Point, finishPort: Point, reversed: Boolean) {

  def startColumn = startPort.column

  def finishColumn = finishPort.column

  def requiresBend = !isStraight

  def isStraight = startColumn == finishColumn

  def withFinishColumn(column: Int) = copy(finishPort = finishPort.withColumn(column))

}
