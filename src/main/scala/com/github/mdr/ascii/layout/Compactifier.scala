package com.github.mdr.ascii.layout

import com.github.mdr.ascii.util.Utils._
import com.github.mdr.ascii.Down

object Compactifier {

  def compactify(drawing: Drawing): Drawing = iterate(drawing, compactOnce)

  def compactOnce(drawing: Drawing): Option[Drawing] = {
    val grid = new OccupancyGrid(drawing)
    for {
      edgeElement ← drawing.elements.collect { case ede: EdgeDrawingElement ⇒ ede }
      updatedElement ← compact(edgeElement, grid)
    } return Some(drawing.replaceElement(edgeElement, updatedElement))
    None
  }

  def compact(edgeElement: EdgeDrawingElement, grid: OccupancyGrid): Option[EdgeDrawingElement] = {
    for {
      (segment1, segment2, segment3) ← adjacentTriples(edgeElement.segments)
      if segment1.direction == Down && segment3.direction == Down
      row ← (segment1.start.row + 2) to (segment2.start.row - 1)
    } {
      val alternativeStart2 = segment2.start.copy(row = row)
      val alternativeFinish2 = segment2.finish.copy(row = row)
      val fakeElement = new EdgeDrawingElement(
        List(alternativeStart2, alternativeFinish2), false, false)
      val allPoints = fakeElement.allPoints
      val newPoints = allPoints.drop(1)
      val allClear = !newPoints.exists(grid.isOccupied)
      if (allClear) {
        println("hit!")
        println(edgeElement)
        val oldBendPoints = edgeElement.points
        val oldIndex = oldBendPoints.indexOf(segment1.finish).ensuring(_ >= 0)
        val newBendPoints = oldBendPoints.patch(oldIndex, List(alternativeStart2, alternativeFinish2), 2)
        val updated = edgeElement.copy(points = newBendPoints)
        println(updated)
        return Some(updated)
      }

    }
    None
  }

}