package com.github.mdr.ascii.layout.drawing

import com.github.mdr.ascii.util.Utils._
import com.github.mdr.ascii.common.Direction._

import scala.annotation.tailrec

/**
 * Delete redundant rows that consist of nothing but vertical edges:
 *
 *    ╭───────╮            ╭───────╮
 *    │   A   │            │   A   │
 *    ╰─┬─┬─┬─╯            ╰─┬─┬─┬─╯
 *      │ │ │                │ │ │
 *   ╭──╯ ╰╮╰────╮        ╭──╯ ╰╮╰────╮
 *   │     │     │   =>   │     │     │
 *   │     │     │        v     v     v
 *   │     │     │      ╭───╮ ╭───╮ ╭───╮
 *   v     v     v      │ B │ │ C │ │ D │
 * ╭───╮ ╭───╮ ╭───╮    ╰───╯ ╰───╯ ╰───╯
 * │ B │ │ C │ │ D │
 * ╰───╯ ╰───╯ ╰───╯
 *
 */
object RedundantRowRemover {

  def removeRedundantRows(drawing: Drawing): Drawing = iterate(drawing, removeRedundantRow)

  private def removeRedundantRow(drawing: Drawing): Option[Drawing] = {
    for (row ← 0 until drawing.dimension.height if canRemove(drawing, row))
      return Some(removeRows(drawing, row, row))
    None
  }

  private def canRemove(drawing: Drawing, row: Int): Boolean =
    drawing.elements.forall {
      case ede: EdgeDrawingElement   ⇒ canRemove(ede, row)
      case vde: VertexDrawingElement ⇒ row < vde.region.topRow || row > vde.region.bottomRow
    }

  private def canRemove(ede: EdgeDrawingElement, row: Int): Boolean = {
    val List(firstBendPoint, secondBendPoint, _*) = ede.bendPoints
    val wouldLeaveStubbyUpArrow =
      row == firstBendPoint.row + 1 &&
        ede.hasArrow1 &&
        secondBendPoint.row == row + 1 &&
        ede.bendPoints.size > 2

    val List(lastBendPoint, secondLastBendPoint, _*) = ede.bendPoints.reverse
    val wouldLeaveStubbyDownArrow =
      row == lastBendPoint.row - 1 &&
        ede.hasArrow2 &&
        secondLastBendPoint.row == row - 1 &&
        ede.bendPoints.size > 2

    !wouldLeaveStubbyDownArrow && !wouldLeaveStubbyUpArrow && ede.bendPoints.forall { _.row != row }
  }

  private def removeRows(drawing: Drawing, fromRow: Int, toRow: Int): Drawing = {
    val upShift = (toRow - fromRow + 1)
    val newElements = drawing.elements map {
      case ede: EdgeDrawingElement ⇒
        val newBendPoints = conditionallyMap(ede.bendPoints) {
          case p if p.row >= fromRow ⇒ p.up(upShift)
        }
        ede.copy(bendPoints = newBendPoints)
      case vde: VertexDrawingElement ⇒
        if (vde.region.topRow < fromRow)
          vde
        else
          vde.up(upShift)
    }
    drawing.copy(elements = newElements)
  }

}