package com.github.mdr.ascii.layout

import com.github.mdr.ascii.Dimension
import com.github.mdr.ascii.Point
import com.github.mdr.ascii.util.Utils._
import scala.annotation.tailrec
import com.github.mdr.ascii.Direction

object KinkRemover {

  def removeKinks(drawing: Drawing): Drawing = iterate(drawing, removeKink)

  private def removeKink(drawing: Drawing): Option[Drawing] = {
    val grid = new OccupancyGrid(drawing)
    for {
      edgeElement ← drawing.elements.collect { case ede: EdgeDrawingElement ⇒ ede }
      updatedElement ← removeKink(edgeElement, grid)
    } return Some(replaceElement(drawing, edgeElement, replacement = updatedElement))
    None
  }

  private def replaceElement(drawing: Drawing, element: DrawingElement, replacement: DrawingElement) =
    drawing.copy(elements = replacement :: drawing.elements.filterNot(_ == element))

  private def allPairs[T](xs: List[T]): List[(T, T)] = xs zip xs.drop(1)

  private def removeKink(element: EdgeDrawingElement, grid: OccupancyGrid): Option[EdgeDrawingElement] = {
    val segments: List[EdgeSegment] = element.segments.drop(2).dropRight(1)

    for ((segment1 @ EdgeSegment(start, _, middle), segment2 @ EdgeSegment(_, _, end)) ← allPairs(segments)) {
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

}

class OccupancyGrid(drawing: Drawing) {

  private val grid: Array[Array[Boolean]] = Array.fill(drawing.dimension.height, drawing.dimension.width)(false)

  drawing.elements.foreach(record)

  def apply(point: Point): Boolean = grid(point.row)(point.column)

  def isOccupied(point: Point) = this(point)

  private def record(drawingElement: DrawingElement) = drawingElement.points.foreach(markAsOccupied)

  private def markAsOccupied(point: Point) {
    grid(point.row)(point.column) = true
  }

}

