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

    val grid = new OccupancyGrid(drawing.dimension)

    def removeKink(element: EdgeDrawingElement): EdgeDrawingElement = {
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
          return updated
        }
      }
      element
    }
    drawing.elements.foreach(grid.record)
    drawing.copy(elements =
      drawing.elements.map {
        case vde: VertexDrawingElement ⇒ vde
        case ede: EdgeDrawingElement   ⇒ removeKink(ede)
      })
  }

}