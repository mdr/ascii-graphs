package com.github.mdr.ascii.layout.drawing

/**
 * Information about a horizontal edge segment (segment2) and the previous and next vertical segments
 * (segment2 and segment3 respectively).
 *
 *      │
 *    1 │      2
 *      ╰────────────╮
 *                   │ 3
 *                   v
 */
case class EdgeSegmentInfo(
    edgeElement: EdgeDrawingElement, segment1: EdgeSegment, segment2: EdgeSegment, segment3: EdgeSegment) {

  def row = segment2.start.row

  /**
   * Shift segment2 to the given row, and adjust segment1 and segment3 ot match.
   */
  def withRow(row: Int): EdgeSegmentInfo = {
    val newStart2 = segment2.start.copy(row = row)
    val newFinish2 = segment2.finish.copy(row = row)
    val newSegment1 = segment1.copy(finish = newStart2)
    val newSegment2 = segment2.copy(start = newStart2, finish = newFinish2)
    val newSegment3 = segment3.copy(start = newFinish2)
    EdgeSegmentInfo(edgeElement, newSegment1, newSegment2, newSegment3)
  }

}