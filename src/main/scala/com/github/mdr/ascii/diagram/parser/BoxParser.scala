package com.github.mdr.ascii.diagram.parser

import scala.annotation.tailrec
import scala.PartialFunction.cond
import com.github.mdr.ascii.common._
import com.github.mdr.ascii.common.Direction._
import com.github.mdr.ascii.layout.drawing.BoxDrawingCharacters._

/**
 * Responsible for detecting boxes in the diagram, e.g.
 *
 *   +---+      ╭───╮
 *   | A |  or  │ A │
 *   +---+      ╰───╯
 *
 */
trait BoxParser { self: DiagramParser ⇒

  protected def findAllBoxes: List[BoxImpl] =
    for {
      topLeft ← possibleTopLefts
      bottomRight ← completeBox(topLeft)
    } yield new BoxImpl(topLeft, bottomRight)

  private def possibleTopLefts: List[Point] =
    for {
      row ← (0 until numberOfRows - 1).toList
      column ← 0 until numberOfColumns - 1
      point = Point(row, column)
      cornerChar = charAt(point)
      if isTopLeftCorner(cornerChar)
      if isHorizontalBoxEdge(charAt(point go Right)) || isBoxDrawingCharacter(cornerChar)
      if isVerticalBoxEdge(charAt(point go Down)) || isBoxDrawingCharacter(cornerChar)
      // +-++-+
      // |A||B| Avoid closely-packed ASCII boxes parsing as 3 boxes, not 2
      // +-++-+
    } yield point

  @tailrec
  private def scanBoxEdge(p: Point, dir: Direction, isCorner: Char ⇒ Boolean, isEdge: Char ⇒ Boolean): Option[Point] =
    if (inDiagram(p)) {
      val c = charAt(p)
      if (isCorner(c))
        Some(p)
      else if (isEdge(c))
        scanBoxEdge(p.go(dir), dir, isCorner, isEdge)
      else
        None
    } else
      None // Can I use charAtOpt + flatMap and keep tailrec?

  /**
   * @return bottomRight of a box if all the edges are filled in correctly.
   */
  private def completeBox(topLeft: Point): Option[Point] =
    for {
      topRight ← scanBoxEdge(topLeft.right, Right, isTopRightCorner, isHorizontalBoxEdge)
      bottomRight ← scanBoxEdge(topRight.down, Down, isBottomRightCorner, isVerticalBoxEdge)
      bottomLeft ← scanBoxEdge(topLeft.down, Down, isBottomLeftCorner, isVerticalBoxEdge)
      bottomRight2 ← scanBoxEdge(bottomLeft.right, Right, isBottomRightCorner, isHorizontalBoxEdge)
      if bottomRight == bottomRight2 // sanity check
    } yield bottomRight

  private def isTopRightCorner(c: Char): Boolean = cond(c) { case '╗' | '╮' | '┐' | '+' ⇒ true }

  private def isBottomRightCorner(c: Char): Boolean = cond(c) { case '╝' | '╯' | '┘' | '+' ⇒ true }

  private def isTopLeftCorner(c: Char): Boolean = cond(c) { case '╔' | '╭' | '┌' | '+' ⇒ true }

  private def isBottomLeftCorner(c: Char): Boolean = cond(c) { case '╚' | '╰' | '└' | '+' ⇒ true }

  private def isHorizontalBoxEdge(c: Char): Boolean = cond(c) {
    case '═' | '─' | '-' | '╤' | '┬' | '╧' | '┴' | '╪' | '┼' ⇒ true
  }

  private def isVerticalBoxEdge(c: Char): Boolean = cond(c) {
    case '║' | '│' | '|' | '╢' | '┤' | '╟' | '├' | '╫' | '┼' ⇒ true
  }

}