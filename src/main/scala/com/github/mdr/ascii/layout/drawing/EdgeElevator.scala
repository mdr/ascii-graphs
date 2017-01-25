package com.github.mdr.ascii.layout.drawing

import com.github.mdr.ascii.util.Utils._
import com.github.mdr.ascii.common.Direction._
import com.github.mdr.ascii.common.Region
import scala.annotation.tailrec

/**
 * Raise edges if there are no conflicting diagram elements. For example:
 *
 *      ╭───────╮           ╭───────╮
 *      │   A   │           │   A   │
 *      ╰─┬─┬─┬─╯           ╰─┬─┬─┬─╯
 *        │ │ │               │ │ │
 *        │ │ ╰────╮       ╭──╯ ╰╮╰────╮
 *        │ ╰╮     │   =>  │     │     │
 *     ╭──╯  │     │       │     │     │
 *     │     │     │       │     │     │
 *     v     v     v       v     v     v
 *   ╭───╮ ╭───╮ ╭───╮   ╭───╮ ╭───╮ ╭───╮
 *   │ B │ │ C │ │ D │   │ B │ │ C │ │ D │
 *   ╰───╯ ╰───╯ ╰───╯   ╰───╯ ╰───╯ ╰───╯
 *
 */
object EdgeElevator {

  def elevateEdges(drawing: Drawing): Drawing = {
    val edgeTracker = new EdgeTracker(drawing)
    var currentDrawing = drawing

    val segmentInfos = for {
      edgeElement ← drawing.edgeElements
      triple @ (segment1, segment2, segment3) ← adjacentTriples(edgeElement.segments)
      if segment2.direction.isHorizontal
    } yield EdgeSegmentInfo(edgeElement, segment1, segment2, segment3)

    var segmentUpdates: Map[EdgeDrawingElement, List[(EdgeSegment, EdgeSegment)]] = Map()
    for {
      segmentInfo ← segmentInfos.sortBy(_.row)
      updatedEdgeSegment ← elevate(segmentInfo, edgeTracker)
    } segmentUpdates = addToMultimap(segmentUpdates, segmentInfo.edgeElement, segmentInfo.segment2 → updatedEdgeSegment)

    for ((edge, updates) ← segmentUpdates)
      currentDrawing = currentDrawing.replaceElement(edge, updateEdge(edge, updates))

    currentDrawing
  }

  @tailrec
  private def updateEdge(edge: EdgeDrawingElement, updates: List[(EdgeSegment, EdgeSegment)]): EdgeDrawingElement =
    updates match {
      case Nil                              ⇒ edge
      case (oldSegment, newSegment) :: rest ⇒ updateEdge(edge.replaceSegment(oldSegment, newSegment), rest)
    }

  private def elevate(segmentInfo: EdgeSegmentInfo, edgeTracker: EdgeTracker): Option[EdgeSegment] = {
    import segmentInfo._
    val firstRow = segmentInfo.segment1.start.row + 1 /* 2 */
    val lastRow = segmentInfo.segment2.start.row - 1
    for {
      row ← firstRow to lastRow
      segment ← elevate(row, segmentInfo, edgeTracker)
    } return Some(segment)
    None
  }

  private def elevate(row: Int, segmentInfo: EdgeSegmentInfo, edgeTracker: EdgeTracker): Option[EdgeSegment] = {
    edgeTracker.removeEdgeSegments(segmentInfo)
    val newSegmentInfo = segmentInfo.withRow(row)
    if (edgeTracker collidesWith newSegmentInfo) {
      edgeTracker.addEdgeSegments(segmentInfo)
      None
    } else {
      edgeTracker.addEdgeSegments(newSegmentInfo)
      Some(newSegmentInfo.segment2)
    }
  }

}
