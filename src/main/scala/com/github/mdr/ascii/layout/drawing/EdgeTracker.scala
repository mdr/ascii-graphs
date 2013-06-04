package com.github.mdr.ascii.layout.drawing

import com.github.mdr.ascii.common.Region

/**
 * Keep track of vertex regions, and horizontal and vertical edge segments as we move them around, so we can detect
 *  collisions.
 */
class EdgeTracker(drawing: Drawing) {

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

  private var horizontalEdgeSegments: Set[Region] = Set()

  private var verticalEdgeSegments: Set[Region] = Set()

  for {
    edge ← drawing.edgeElements
    segment ← edge.segments
  } {
    if (segment.direction.isHorizontal)
      horizontalEdgeSegments += segment.region
    else
      verticalEdgeSegments += segment.region
  }

  def addEdgeSegments(segmentInfo: EdgeSegmentInfo) {
    verticalEdgeSegments += segmentInfo.segment1.region
    horizontalEdgeSegments += segmentInfo.segment2.region
    verticalEdgeSegments += segmentInfo.segment3.region
  }

  def removeEdgeSegments(segmentInfo: EdgeSegmentInfo) {
    verticalEdgeSegments -= segmentInfo.segment1.region
    horizontalEdgeSegments -= segmentInfo.segment2.region
    verticalEdgeSegments -= segmentInfo.segment3.region
  }

  private def collidesHorizontal(region: Region): Boolean =
    vertexRegions.exists(region.intersects) || arrowRegions.exists(region.intersects) || horizontalEdgeSegments.exists(region.intersects)

  private def collidesVertical(region: Region): Boolean =
    vertexRegions.exists(region.intersects) || verticalEdgeSegments.exists(region.intersects)

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
    val vertexDrawingElements = vertexRegions.map(VertexDrawingElement(_, Nil))
    def drawSegment(r: Region) = EdgeDrawingElement(List(r.topLeft, r.bottomRight), false, false)
    val drawing = Drawing(vertexDrawingElements ++
      horizontalEdgeSegments.map(drawSegment) ++ verticalEdgeSegments.map(drawSegment))
    drawing.toString
  }

}
