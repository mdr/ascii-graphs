package com.github.mdr.ascii.diagram.parser

import com.github.mdr.ascii._
import com.github.mdr.ascii.common.Direction._
import com.github.mdr.ascii.common._
import com.github.mdr.ascii.diagram._
import scala.annotation.tailrec
import scala.PartialFunction.cond

/**
 * Follow an edge drawn using Unicode box-drawing characters.
 */
trait UnicodeEdgeParser { self: DiagramParser ⇒

  /**
   * @param points -- non-empty list of points in the edge so far, in reverse order
   * @param direction -- direction taken to reach the tip of the edge
   * @return Some(edge) if the edge is well-formed and terminates at a box, otherwise None.
   */
  @tailrec
  protected final def followUnicodeEdge(points: List[Point], direction: Direction): Option[EdgeImpl] = {
    val currentPoint = points.head
    if (!inDiagram(currentPoint))
      return None
    if (isBoxEdge(currentPoint))
      return Some(new EdgeImpl(points.reverse))
    val c = charAt(currentPoint)
    if (isStraightAhead(c, direction) || isCrossing(c) || isAheadArrow(c, direction))
      followUnicodeEdge(currentPoint.go(direction) :: points, direction)
    else if (isLeftTurn(c, direction) || isLeftArrow(c, direction))
      followUnicodeEdge(currentPoint.go(direction.turnLeft) :: points, direction.turnLeft)
    else if (isRightTurn(c, direction) || isRightArrow(c, direction))
      followUnicodeEdge(currentPoint.go(direction.turnRight) :: points, direction.turnRight)
    else
      None
  }

  protected def isEdgeStart(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('╤' | '┬', Down)         ⇒ true
    case ('╪' | '┼', Up | Down)    ⇒ true
    case ('╧' | '┴', Up)           ⇒ true
    case ('╟' | '├', Right)        ⇒ true
    case ('╫' | '┼', Right | Left) ⇒ true
    case ('╢' | '┤', Left)         ⇒ true
  }

  private def isStraightAhead(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('─', Right | Left) ⇒ true
    case ('│', Up | Down)    ⇒ true
  }

  private def isLeftArrow(c: Char, direction: Direction) = isAheadArrow(c, direction.turnLeft)

  private def isRightArrow(c: Char, direction: Direction) = isAheadArrow(c, direction.turnRight)

  private def isAheadArrow(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('^', Up)         ⇒ true
    case ('v' | 'V', Down) ⇒ true
    case ('<', Left)       ⇒ true
    case ('>', Right)      ⇒ true
  }

  private def isRightTurn(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('╮' | '┐', Right) ⇒ true
    case ('╯' | '┘', Down)  ⇒ true
    case ('╭' | '┌', Up)    ⇒ true
    case ('╰' | '└', Left)  ⇒ true
  }

  private def isLeftTurn(c: Char, direction: Direction): Boolean = isRightTurn(c, direction.turnRight)

  private def isCrossing(c: Char): Boolean = c == '┼'

}