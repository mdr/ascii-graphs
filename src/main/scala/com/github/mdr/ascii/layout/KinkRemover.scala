package com.github.mdr.ascii.layout

import com.github.mdr.ascii.Dimension
import com.github.mdr.ascii.Point

object KinkRemover {

  private class OccupancyGrid(dimension: Dimension) {

    private val grid: Array[Array[Boolean]] = Array.fill(dimension.height, dimension.width)(false)

    def apply(point: Point): Boolean = grid(point.row)(point.column)

    def isOccupied(point: Point) = this(point)

    def record(drawingElement: DrawingElement) = drawingElement.points.foreach(markAsOccupied)

    private def markAsOccupied(point: Point) {
      grid(point.row)(point.column) = true
    }

  }

  def removeKinks(drawing: Drawing): Drawing = {

    def removeKink(drawing: Drawing): Option[Drawing] = {

      val grid = new OccupancyGrid(drawing.dimension)
      drawing.elements.foreach(grid.record)

      def removeKink(element: EdgeDrawingElement): Option[EdgeDrawingElement] = {
        val segmentPairs = element.pointAndDirections.drop(2).dropRight(1).sliding(2, 2).toList
        for (List(segment1 @ (start, _, middle), segment2 @ (_, _, end)) ← segmentPairs) {
          val alternativeMiddle = Point(start.row, end.column)
          val fakeElement = new EdgeDrawingElement(List(start, alternativeMiddle, end), false, false)
          val newPoints = fakeElement.allPoints.drop(1).dropRight(1)
          val allClear = !newPoints.exists(grid.isOccupied)
          if (allClear) {
            val oldBendPoints = element.points
            val oldIndex = oldBendPoints.indexOf(start).ensuring(_ >= 0)
            val newBendPoints = oldBendPoints.patch(oldIndex, List(alternativeMiddle), 3)

            val updated = element.copy(points = newBendPoints)
            return Some(updated)
          }
        }
        None
      }

      for {
        el ← drawing.elements.collect { case ede: EdgeDrawingElement ⇒ ede }
        updatedEl ← removeKink(el).toList
      } return Some(Drawing(updatedEl :: drawing.elements.filterNot(_ == el)))
      None
    }

    var drawing_ = drawing
    while (true) {
      removeKink(drawing_) match {
        case None    ⇒ return drawing_
        case Some(d) ⇒ drawing_ = d
      }
    }
    return drawing_
  }

}