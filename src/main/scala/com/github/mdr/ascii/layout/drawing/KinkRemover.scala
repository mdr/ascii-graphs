package com.github.mdr.ascii.layout.drawing

import scala.annotation.tailrec
import com.github.mdr.ascii.common.Direction._
import com.github.mdr.ascii.common.Point
import com.github.mdr.ascii.util.Utils._
import com.github.mdr.ascii.common.Direction

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

      // ...──start──╮.............. alternativeMiddle
      //             │             .
      //   segment1  │             .
      //             │             .
      //     middle  ╰─────────────╮ end
      //                segment2   │
      //                           .
      //                           .
      case (segment1 @ EdgeSegment(start, Down, middle), segment2 @ EdgeSegment(_, Left | Right, end)) ⇒
        val alternativeMiddle = Point(start.row, end.column)
        val fakeEdge = new EdgeDrawingElement(List(start, alternativeMiddle, end), false, false)
        val newPoints = fakeEdge.points.filterNot(edge.points.contains)
        val allPointsVacant = !newPoints.exists(grid.isOccupied)
        if (allPointsVacant && checkVertexConnection(drawing, start, alternativeMiddle, Direction.Up))
          return Some(removeKink(edge, start, alternativeMiddle))

      //                    .
      //                    .
      //                    │  segment1
      //              start ╰────────────╮  middle 
      //                    .            │
      //                    .            │  segment2
      //                    .            │  
      //  alternativeMiddle .............╰──end──...
      //                       
      case (segment1 @ EdgeSegment(start, Left | Right, middle), segment2 @ EdgeSegment(_, Down, end)) ⇒
        val alternativeMiddle = Point(end.row, start.column)
        val fakeEdge = new EdgeDrawingElement(List(start, alternativeMiddle, end), false, false)
        val newPoints = fakeEdge.points.filterNot(edge.points.contains)
        val allPointsVacant = !newPoints.exists(grid.isOccupied)
        if (allPointsVacant && checkVertexConnection(drawing, end, alternativeMiddle, Direction.Down))
          return Some(removeKink(edge, start, alternativeMiddle))
    }
    None
  }

  /**
   * Check that the vertex connection won't be changed, and that we don't want to connect to the extreme
   * left or right of the vertex.
   */
  private def checkVertexConnection(drawing: Drawing, end: Point, alternativeMiddle: Point, direction: Direction): Boolean = {
    drawing.vertexElementAt(end.go(direction)).forall { vertex ⇒
      val connectedToSameVertex = drawing.vertexElementAt(alternativeMiddle.go(direction)) == Some(vertex)
      val extremeLeftOfVertex = alternativeMiddle.column == vertex.region.leftColumn
      val extremeRightOfVertex = alternativeMiddle.column == vertex.region.rightColumn
      connectedToSameVertex && !extremeLeftOfVertex && !extremeRightOfVertex
    }
  }

  private def removeKink(edge: EdgeDrawingElement, start: Point, alternativeMiddle: Point): EdgeDrawingElement = {
    val oldBendPoints = edge.bendPoints
    val oldIndex = oldBendPoints.indexOf(start)
    val newBendPoints = Point.removeRedundantPoints(oldBendPoints.patch(oldIndex, List(alternativeMiddle), 3).distinct)
    return edge.copy(bendPoints = newBendPoints)
  }

}
