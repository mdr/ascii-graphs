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

  private def removeKink(edge: EdgeDrawingElement, drawing: Drawing, grid: OccupancyGrid): Option[EdgeDrawingElement] = {
    val segments: List[EdgeSegment] = edge.segments
    adjacentPairs(segments) collect {

      // ...-- start -.... alternativeMiddle
      //             |   .
      //      middle ----- end
      //                 |
      //                 .
      //                 .
      //                 .

      case (segment1 @ EdgeSegment(start, Down, middle), segment2 @ EdgeSegment(_, Left | Right, end)) ⇒
        val alternativeMiddle = Point(start.row, end.column)
        val fakeEdge = new EdgeDrawingElement(List(start, alternativeMiddle, end), false, false)
        val newPoints = fakeEdge.points.filterNot(edge.points.contains)
        val allClear = !newPoints.exists(grid.isOccupied)
        if (allClear) {
          if (segment1 != edge.segments.head ||
            drawing.vertexElementAt(start.up) == drawing.vertexElementAt(alternativeMiddle.up) &&
            drawing.vertexElementAt(start.up).get.region.rightColumn != alternativeMiddle.column &&
            drawing.vertexElementAt(start.up).get.region.leftColumn != alternativeMiddle.column) {
            val oldBendPoints = edge.bendPoints
            val oldIndex = oldBendPoints.indexOf(start)
            val newBendPoints = Point.removeRedundantPoints(oldBendPoints.patch(oldIndex, List(alternativeMiddle), 3).distinct)
            val updated = edge.copy(bendPoints = newBendPoints)
            return Some(updated)
          }
        }

      case (segment1 @ EdgeSegment(start, Left | Right, middle), segment2 @ EdgeSegment(_, Down, end)) ⇒
        val alternativeMiddle = Point(end.row, start.column)
        val fakeEdge = new EdgeDrawingElement(List(start, alternativeMiddle, end), false, false)
        val newPoints = fakeEdge.points.filterNot(edge.points.contains)
        val allClear = !newPoints.exists(grid.isOccupied)
        if (allClear) {
          if (segment2 != edge.segments.last ||
            drawing.vertexElementAt(end.down) == drawing.vertexElementAt(alternativeMiddle.down) &&
            drawing.vertexElementAt(end.down).get.region.rightColumn != alternativeMiddle.column &&
            drawing.vertexElementAt(end.down).get.region.leftColumn != alternativeMiddle.column) {
            val oldBendPoints = edge.bendPoints
            val oldIndex = oldBendPoints.indexOf(start)
            val newBendPoints = Point.removeRedundantPoints(oldBendPoints.patch(oldIndex, List(alternativeMiddle), 3).distinct)
            val updated = edge.copy(bendPoints = newBendPoints)
            return Some(updated)
          }
        }
    }
    None
  }

}
