package com.github.mdr.ascii.layout.drawing

import scala.annotation.tailrec

import com.github.mdr.ascii.common.Direction._
import com.github.mdr.ascii.common.Point
import com.github.mdr.ascii.util.Utils._

/**
 * Remove kinks in edges where this can be achieved by removing an edge segment. For example:
 *
 *  ╭───────────╮        ╭───────────╮
 *  │Aberystwyth│        │Aberystwyth│
 *  ╰─────┬─────╯        ╰─┬─────────╯
 *        │        ==>     │
 *    ╭───╯                │
 *    │                    │
 *    v                    v
 *  ╭───╮                ╭───╮
 *  │ X │                │ X │
 *  ╰───╯                ╰───╯
 */
object KinkRemover {

  // def removeKinks(drawing: Drawing) = iterate(drawing, removeKink)
  //
  //  private def removeKink(drawing: Drawing): Option[Drawing] = {
  //    val grid = new OccupancyGrid(drawing)
  //    for {
  //      edgeElement ← drawing.edgeElements
  //      updatedElement ← removeKink(edgeElement, drawing, grid)
  //    } return Some(drawing.replaceElement(edgeElement, updatedElement))
  //    None
  //  }

  def removeKinks(drawing: Drawing): Drawing = {
    val grid = new OccupancyGrid(drawing)
    var currentDrawing = drawing
    var continue = true
    while (continue)
      removeKink(currentDrawing, grid) match {
        case None ⇒
          continue = false
        case Some((oldEdge, updatedEdge)) ⇒
          currentDrawing = currentDrawing.replaceElement(oldEdge, updatedEdge)
          grid.replace(oldEdge, updatedEdge)
      }
    return currentDrawing
  }

  private def removeKink(drawing: Drawing, grid: OccupancyGrid): Option[(EdgeDrawingElement, EdgeDrawingElement)] = {
    for {
      edgeElement ← drawing.edgeElements
      newEdgeElement ← removeKink(edgeElement, drawing, grid)
    } return Some(edgeElement -> newEdgeElement)
    None
  }

  private def sameColumn(p1: Point, p2: Point, p3: Point) = p1.column == p2.column && p2.column == p3.column
  private def sameRow(p1: Point, p2: Point, p3: Point) = p1.row == p2.row && p2.row == p3.row
  private def colinear(p1: Point, p2: Point, p3: Point) = sameColumn(p1, p2, p3) || sameRow(p1, p2, p3)

  private def removeRedundantPoints(points: List[Point]): List[Point] = points match {
    case List() | List(_) | List(_, _)                       ⇒ points
    case p1 :: p2 :: p3 :: remainder if colinear(p1, p2, p3) ⇒ removeRedundantPoints(p1 :: p3 :: remainder)
    case p :: ps                                             ⇒ p :: removeRedundantPoints(ps)
  }

  private def removeKink(element: EdgeDrawingElement, drawing: Drawing, grid: OccupancyGrid): Option[EdgeDrawingElement] = {
    val segments: List[EdgeSegment] = element.segments
    adjacentPairs(segments) collect {
      case (segment1 @ EdgeSegment(start, Down, middle), segment2 @ EdgeSegment(_, Left | Right, end)) ⇒
        val alternativeMiddle = Point(start.row, end.column)
        val fakeElement = new EdgeDrawingElement(List(start, alternativeMiddle, end), false, false)
        val newPoints = fakeElement.points.filterNot(element.points.contains)
        val allClear = !newPoints.exists(grid.isOccupied)
        if (allClear) {
          val originalStartVertex = drawing.vertexElementAt(start)
          val newStartVertex = drawing.vertexElementAt(alternativeMiddle)
          if (segment1 != element.segments.head ||
            drawing.vertexElementAt(start.up) == drawing.vertexElementAt(alternativeMiddle.up) &&
            drawing.vertexElementAt(start.up).get.region.rightColumn != alternativeMiddle.column &&
            drawing.vertexElementAt(start.up).get.region.leftColumn != alternativeMiddle.column) {
            val oldBendPoints = element.bendPoints
            val oldIndex = oldBendPoints.indexOf(start).ensuring(_ >= 0)
            val newBendPoints = removeRedundantPoints(oldBendPoints.patch(oldIndex, List(alternativeMiddle), 3).distinct)
            val updated = element.copy(bendPoints = newBendPoints)
            return Some(updated)
          }
        }
      case (segment1 @ EdgeSegment(start, Left | Right, middle), segment2 @ EdgeSegment(_, Down, end)) ⇒
        val alternativeMiddle = Point(end.row, start.column)
        val fakeElement = new EdgeDrawingElement(List(start, alternativeMiddle, end), false, false)
        val newPoints = fakeElement.points.filterNot(element.points.contains)
        val allClear = !newPoints.exists(grid.isOccupied)
        if (allClear) {
          val originalStartVertex = drawing.vertexElementAt(end)
          val newStartVertex = drawing.vertexElementAt(alternativeMiddle)
          if (segment2 != element.segments.last ||
            drawing.vertexElementAt(end.down) == drawing.vertexElementAt(alternativeMiddle.down) &&
            drawing.vertexElementAt(end.down).get.region.rightColumn != alternativeMiddle.column &&
            drawing.vertexElementAt(end.down).get.region.leftColumn != alternativeMiddle.column) {
            val oldBendPoints = element.bendPoints
            val oldIndex = oldBendPoints.indexOf(start).ensuring(_ >= 0)
            val newBendPoints = removeRedundantPoints(oldBendPoints.patch(oldIndex, List(alternativeMiddle), 3).distinct)
            val updated = element.copy(bendPoints = newBendPoints)
            return Some(updated)
          }
        }
    }
    None
  }

}
