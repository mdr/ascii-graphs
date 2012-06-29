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
    } {
      println("Shifted edge up in " + edgeElement + ", " + updatedElement)
      val updatedDrawing = drawing.replaceElement(edgeElement, updatedElement)
      return Some(updatedDrawing)
    }
    None
  }

  def compact(edgeElement: EdgeDrawingElement, grid: OccupancyGrid): Option[EdgeDrawingElement] = {
    for {
      (segment1, segment2, segment3) ← adjacentTriples(edgeElement.segments)
      if segment1.direction == Down && segment3.direction == Down
      row ← (segment1.start.row + /* 2 */ 1) to (segment2.start.row - 1)
    } {
      val alternativeStart2 = segment2.start.copy(row = row)
      val alternativeFinish2 = segment2.finish.copy(row = row)
      val fakeElement = new EdgeDrawingElement(
        List(segment1.start, alternativeStart2, alternativeFinish2, segment3.finish), false, false)
      val newPoints = fakeElement.points.filterNot(edgeElement.points.contains)
      val allClear = !newPoints.exists(grid.isOccupied)
      if (allClear) {
        val oldBendPoints = edgeElement.bendPoints
        val oldIndex = oldBendPoints.indexOf(segment2.start).ensuring(_ >= 0)
        val newBendPoints = oldBendPoints.patch(oldIndex, List(alternativeStart2, alternativeFinish2), 2)
        val updated = edgeElement.copy(bendPoints = newBendPoints)
        return Some(updated)
      }

    }
    None
  }

}