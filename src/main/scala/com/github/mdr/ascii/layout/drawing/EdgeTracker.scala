package com.github.mdr.ascii.layout.drawing

import com.github.mdr.ascii.common.Region
import com.github.mdr.ascii.util.QuadTree

/**
 * Keep track of vertex regions, and horizontal and vertical edge segments as we move them around, so we can detect
 *  collisions.
 */
class EdgeTracker(drawing: Drawing) {

  private val horizontalQuadTree: QuadTree[Region] = new QuadTree(drawing.dimension)
  private val verticalQuadTree: QuadTree[Region] = new QuadTree(drawing.dimension)

  private val vertexRegions: List[Region] = drawing.vertexElements.map(_.region)

  /**
   * The (single cell) regions containing edge arrows.
   */
  private val arrowRegions: List[Region] = drawing.edgeElements.map { edgeElement ⇒
    if (edgeElement.hasArrow1)
      edgeElement.startPoint.region
    else
      edgeElement.finishPoint.region
  }

  arrowRegions.foreach(horizontalQuadTree.add)

  vertexRegions.foreach(horizontalQuadTree.add)
  vertexRegions.foreach(verticalQuadTree.add)

  for {
    edge ← drawing.edgeElements
    segment ← edge.segments
  } {
    if (segment.direction.isHorizontal)
      horizontalQuadTree.add(segment.region)
    else
      verticalQuadTree.add(segment.region)
  }

  def addEdgeSegments(segmentInfo: EdgeSegmentInfo) {
    verticalQuadTree.add(segmentInfo.segment1.region)
    horizontalQuadTree.add(segmentInfo.segment2.region)
    verticalQuadTree.add(segmentInfo.segment3.region)
  }

  def removeEdgeSegments(segmentInfo: EdgeSegmentInfo) {
    verticalQuadTree.remove(segmentInfo.segment1.region)
    horizontalQuadTree.remove(segmentInfo.segment2.region)
    verticalQuadTree.remove(segmentInfo.segment3.region)
  }

  def addHorizontalSegment(edgeSegment: EdgeSegment) {
    horizontalQuadTree.add(edgeSegment.region)
  }

  def addVerticalSegment(edgeSegment: EdgeSegment) {
    verticalQuadTree.add(edgeSegment.region)
  }

  def removeHorizontalSegment(edgeSegment: EdgeSegment) {
    horizontalQuadTree.remove(edgeSegment.region)
  }

  def removeVerticalSegment(edgeSegment: EdgeSegment) {
    verticalQuadTree.remove(edgeSegment.region)
  }

  def collidesHorizontal(edgeSegment: EdgeSegment): Boolean = collidesHorizontal(edgeSegment.region)

  def collidesVertical(edgeSegment: EdgeSegment): Boolean = collidesVertical(edgeSegment.region)

  private def collidesHorizontal(region: Region): Boolean = horizontalQuadTree.collides(region)

  private def collidesVertical(region: Region): Boolean = verticalQuadTree.collides(region)

  /**
   * Check that the segments won't overwrite existing segments, collide with vertices or arrows, and that it won't cause
   * any new twists". Example of the latter:
   *              ╭─────╮            ╭─────╮
   *              │  A  │            │  A  │
   *              ╰─┬─┬─╯            ╰─┬─┬─╯
   *                │ │                │ │
   *                │ │   vs         ╭─┼─╯
   *        ╭───────╯ │        ╭─────┼─╯
   *        │     ╭───╯        │     │
   *        │     │            │     │
   *        v     v            v     v
   *  ╭───────╮ ╭───╮    ╭───────╮ ╭───╮
   *  │   F   │ │ G │    │   F   │ │ G │
   *  ╰───────╯ ╰───╯    ╰───────╯ ╰───╯
   */
  def collidesWith(segmentInfo: EdgeSegmentInfo): Boolean =
    collidesVertical(segmentInfo.segment1.region) ||
      collidesHorizontal(segmentInfo.segment2.region) ||
      collidesVertical(segmentInfo.segment3.region) ||
      collidesVertical(segmentInfo.segment2.region) && collidesHorizontal(segmentInfo.segment3.region)

  override def toString = {
    horizontalQuadTree + "\n------------------------------------------------------------\n" + verticalQuadTree
  }
}
