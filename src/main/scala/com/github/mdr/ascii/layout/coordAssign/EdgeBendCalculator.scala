package com.github.mdr.ascii.layout.coordAssign

/**
 * Calculate vertical ordering of row bends
 */
class EdgeBendCalculator(edgeInfos: List[EdgeInfo], edgeZoneTopRow: Int, selfEdgeBuffer: Int) {

  private val edgeRows: Map[EdgeInfo, Int] = orderEdgeBends(edgeInfos)
  private def bendRow(rowIndex: Int) = edgeZoneTopRow + rowIndex * 1 + 1

  val edgeZoneBottomRow =
    (if (edgeInfos.isEmpty) // No edges
      -1
    else if (edgeRows.isEmpty) // No edges with bends
      edgeZoneTopRow + 2
    else
      bendRow(edgeRows.values.max) + 2) + selfEdgeBuffer

  def bendRow(edgeInfo: EdgeInfo): Int = bendRow(edgeRows(edgeInfo))

  /**
   * @return a vertical ordering of those edges that require bends.
   */
  private def orderEdgeBends(edgeInfos: List[EdgeInfo]): Map[EdgeInfo, Int] = {
    // To avoid unnecessary crossings of edges arriving at the same vertex:
    val sortedInfos: List[EdgeInfo] = edgeInfos.sortBy(_.inRank)
    val edgeRows: Map[EdgeInfo, Int] = sortedInfos.filter(_.requiresBend).zipWithIndex.toMap
    reorderEdgesWithSameStartAndEndColumns(edgeRows, sortedInfos)
  }

  /**
   * Force edges that share start and end columns to be ordered so as to avoid conflicts
   */
  private def reorderEdgesWithSameStartAndEndColumns(edgeRows: Map[EdgeInfo, Int], sortedInfos: List[EdgeInfo]): Map[EdgeInfo, Int] = {
    var updatedEdgeRows = edgeRows
    var continue = true
    while (continue) {
      continue = false
      for {
        edgeInfo1 @ EdgeInfo(_, _, start1, finish1, _) ← sortedInfos
        edgeInfo2 @ EdgeInfo(_, _, start2, finish2, _) ← sortedInfos
        if edgeInfo1 != edgeInfo2
        if start1.column == finish2.column
        if start2.column != finish1.column // Prevents an infinite loop (issue #3), but TODO: still allows overlapping edges
        row1 = updatedEdgeRows(edgeInfo1)
        row2 = updatedEdgeRows(edgeInfo2)
        if row1 > row2
      } {
        updatedEdgeRows += edgeInfo1 -> row2
        updatedEdgeRows += edgeInfo2 -> row1
        continue = true
      }
    }
    updatedEdgeRows
  }

}