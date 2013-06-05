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
    val edgeTracker = new KinkEdgeTracker(drawing)
    var currentDrawing = drawing
    var continue = true
    while (continue)
      removeKink(currentDrawing, edgeTracker) match {
        case None ⇒
          continue = false
        case Some((oldEdge, updatedEdge)) ⇒
          currentDrawing = currentDrawing.replaceElement(oldEdge, updatedEdge)
      }
    return currentDrawing
  }

  private def removeKink(drawing: Drawing, edgeTracker: KinkEdgeTracker): Option[(EdgeDrawingElement, EdgeDrawingElement)] = {
    for {
      edgeElement ← drawing.edgeElements
      newEdgeElement ← removeKink(edgeElement, drawing, edgeTracker)
    } return Some(edgeElement -> newEdgeElement)
    None
  }

  private def removeKink(edge: EdgeDrawingElement, drawing: Drawing, edgeTracker: KinkEdgeTracker): Option[EdgeDrawingElement] = {
    val segments: List[EdgeSegment] = edge.segments
    adjacentPairsWithPreviousAndNext(segments) collect {

      //   segment1
      // ...──start─╮.............. alternativeMiddle
      //            │             .
      //   segment2 │             .
      //            │             .
      //     middle ╰─────────────╮ end
      //               segment3   │
      //                          │ segment4
      //                          │
      //                          .
      //                          .
      case (segment1Opt, segment2 @ EdgeSegment(start, Down, middle), segment3 @ EdgeSegment(_, Left | Right, end), segment4Opt) ⇒
        val alternativeMiddle = Point(start.row, end.column)

        segment1Opt.foreach(edgeTracker.removeHorizontalSegment)
        edgeTracker.removeVerticalSegment(segment2)
        edgeTracker.removeHorizontalSegment(segment3)
        segment4Opt.foreach(edgeTracker.removeVerticalSegment)

        val newSegment1Opt = segment1Opt.map { segment1 ⇒
          EdgeSegment(segment1.start, segment1.direction, alternativeMiddle)
        }
        val newSegment4Opt = segment4Opt.map { segment4 ⇒
          EdgeSegment(alternativeMiddle, segment4.direction, segment4.finish)
        }
        val collision = newSegment1Opt.exists(edgeTracker.collidesHorizontal) || newSegment4Opt.exists(edgeTracker.collidesVertical)
        if (!collision && checkVertexConnection(drawing, start, alternativeMiddle, Direction.Up)) {
          segment1Opt.foreach(edgeTracker.addHorizontalSegment)
          segment4Opt.foreach(edgeTracker.addVerticalSegment)
          return Some(removeKink(edge, start, alternativeMiddle))
        } else {
          segment1Opt.foreach(edgeTracker.addHorizontalSegment)
          edgeTracker.addVerticalSegment(segment2)
          edgeTracker.addHorizontalSegment(segment3)
          segment4Opt.foreach(edgeTracker.addVerticalSegment)
        }

      //                    .
      //                    .
      //                    │ 
      //           segment1 │ 
      //                    │  segment2
      //              start ╰────────────╮ middle 
      //                    .            │
      //                    .            │ segment3
      //                    .            │ 
      //  alternativeMiddle .............╰─end───────────...
      //                                      segment4
      //                       
      case (segment1Opt, segment2 @ EdgeSegment(start, Left | Right, middle), segment3 @ EdgeSegment(_, Down, end), segment4Opt) ⇒
        val alternativeMiddle = Point(end.row, start.column)
        segment1Opt.foreach(edgeTracker.removeVerticalSegment)
        edgeTracker.removeHorizontalSegment(segment2)
        edgeTracker.removeVerticalSegment(segment3)
        segment4Opt.foreach(edgeTracker.removeHorizontalSegment)

        val newSegment1Opt = segment1Opt.map { segment1 ⇒
          EdgeSegment(segment1.start, segment1.direction, alternativeMiddle)
        }
        val newSegment4Opt = segment4Opt.map { segment4 ⇒
          EdgeSegment(alternativeMiddle, segment4.direction, segment4.finish)
        }
        val collision = newSegment1Opt.exists(edgeTracker.collidesVertical) || newSegment4Opt.exists(edgeTracker.collidesHorizontal)
        if (!collision && checkVertexConnection(drawing, end, alternativeMiddle, Direction.Down)) {
          segment1Opt.foreach(edgeTracker.addVerticalSegment)
          segment4Opt.foreach(edgeTracker.addHorizontalSegment)
          return Some(removeKink(edge, start, alternativeMiddle))
        } else {
          segment1Opt.foreach(edgeTracker.addVerticalSegment)
          edgeTracker.addHorizontalSegment(segment2)
          edgeTracker.addVerticalSegment(segment3)
          segment4Opt.foreach(edgeTracker.addHorizontalSegment)
        }
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
