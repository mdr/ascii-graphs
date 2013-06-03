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
    } segmentUpdates = addToMultimap(segmentUpdates, segmentInfo.edgeElement, segmentInfo.segment2 -> updatedEdgeSegment)

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
    for (row ← (segment1.start.row + /* 2 */ 1) to (segment2.start.row - 1)) {
      val newStart2 = segment2.start.copy(row = row)
      val newFinish2 = segment2.finish.copy(row = row)

      val newSegment1 = segment1.copy(finish = newStart2)
      val newSegment2 = segment2.copy(start = newStart2, finish = newFinish2)
      val newSegment3 = segment3.copy(start = newFinish2)

      edgeTracker.removeVerticalEdgeSegment(segment1.region)
      edgeTracker.removeHorizontalEdgeSegment(segment2.region)
      edgeTracker.removeVerticalEdgeSegment(segment3.region)

      val collides1 = edgeTracker.collidesVertical(newSegment1.region)
      val collides2 = edgeTracker.collidesHorizontal(newSegment2.region)
      val collides3 = edgeTracker.collidesVertical(newSegment3.region)

      val collides =
        edgeTracker.collidesVertical(newSegment1.region) ||
          edgeTracker.collidesHorizontal(newSegment2.region) ||
          edgeTracker.collidesVertical(newSegment3.region) ||
          edgeTracker.collidesVertical(newSegment2.region) && edgeTracker.collidesHorizontal(newSegment3.region)

      if (collides) {
        edgeTracker.addVerticalEdgeSegment(segment1.region)
        edgeTracker.addHorizontalEdgeSegment(segment2.region)
        edgeTracker.addVerticalEdgeSegment(segment3.region)
      } else {
        edgeTracker.addVerticalEdgeSegment(newSegment1.region)
        edgeTracker.addHorizontalEdgeSegment(newSegment2.region)
        edgeTracker.addVerticalEdgeSegment(newSegment3.region)

        return Some(newSegment2)
      }
    }
    None
  }

}

/**
 * Keep track of vertex regions, and horizontal and vertical edge segments, so we can detect collisions.
 */
class EdgeTracker(drawing: Drawing) {

  private val vertexRegions = drawing.vertexElements.map(_.region)

  private var horizontalEdgeSegments: Set[Region] = Set()

  private var verticalEdgeSegments: Set[Region] = Set()

  private val arrowRegions = drawing.edgeElements.map(arrowRegion)

  private def arrowRegion(edgeElement: EdgeDrawingElement): Region =
    if (edgeElement.hasArrow1) edgeElement.startPoint.region else edgeElement.finishPoint.region

  for {
    edge ← drawing.edgeElements
    segment ← edge.segments
  } {
    if (segment.direction.isHorizontal)
      horizontalEdgeSegments += segment.region
    else
      verticalEdgeSegments += segment.region
  }

  def addVerticalEdgeSegment(newRegion: Region) = verticalEdgeSegments += newRegion
  def removeVerticalEdgeSegment(region: Region) = verticalEdgeSegments -= region
  def addHorizontalEdgeSegment(newRegion: Region) = horizontalEdgeSegments += newRegion
  def removeHorizontalEdgeSegment(region: Region) = horizontalEdgeSegments -= region

  def collidesHorizontal(region: Region) =
    vertexRegions.exists(region.intersects) || arrowRegions.exists(region.intersects) || horizontalEdgeSegments.exists(region.intersects)

  def collidesVertical(region: Region) =
    vertexRegions.exists(region.intersects) || arrowRegions.exists(region.intersects) || verticalEdgeSegments.exists(region.intersects)

  override def toString = {
    val els = vertexRegions.map(VertexDrawingElement(_, Nil))
    def segmentDrawing(r: Region) = EdgeDrawingElement(List(r.topLeft, r.bottomRight), false, false)
    Drawing(vertexRegions.map(VertexDrawingElement(_, Nil)) ++
      horizontalEdgeSegments.map(segmentDrawing) ++
      verticalEdgeSegments.map(segmentDrawing)).toString
  }

}

/**
 * Information about a horizontal edge segment (segment2) and the previous and next vertical segments
 * (segment2 and segment3 respectively).
 *
 *   1 │      2
 *     ╰────────────╮
 *                  │ 3
 */
case class EdgeSegmentInfo(
    edgeElement: EdgeDrawingElement, segment1: EdgeSegment, segment2: EdgeSegment, segment3: EdgeSegment) {

  def row = segment2.start.row

}
