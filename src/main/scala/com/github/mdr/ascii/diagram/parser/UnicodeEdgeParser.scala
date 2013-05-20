package com.github.mdr.ascii.diagram.parser

import scala.annotation.tailrec
import com.github.mdr.ascii._
import com.github.mdr.ascii.common.Direction._
import com.github.mdr.ascii.common._
import com.github.mdr.ascii.diagram._
import scala.PartialFunction.cond

/**
 * Follow an edge drawn using Unicode box-drawing characters.
 */
trait UnicodeEdgeParser { self: DiagramParser ⇒

  @tailrec
  protected final def followUnicodeEdge(points: List[Point], direction: Direction): Option[EdgeImpl] = {
    val currentPoint = points.head
    if (!inDiagram(currentPoint))
      return None
    val c = charAt(currentPoint)
    if (isBoxEdge(currentPoint))
      Some(new EdgeImpl(points.reverse))
    else if (isStraightAhead(c, direction) || isCrossing(c) || isAheadArrow(c, direction))
      followUnicodeEdge(currentPoint.go(direction) :: points, direction)
    else if (isLeftTurn(c, direction))
      followUnicodeEdge(currentPoint.go(direction.turnLeft) :: points, direction.turnLeft)
    else if (isRightTurn(c, direction))
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

  private def isAheadArrow(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('^', Up)         ⇒ true
    case ('v' | 'V', Down) ⇒ true
    case ('<', Left)       ⇒ true
    case ('>', Right)      ⇒ true
  }

  private def isLeftTurn(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('╮' | '┐', Up)    ⇒ true
    case ('╯' | '┘', Right) ⇒ true
    case ('╭' | '┌', Left)  ⇒ true
    case ('╰' | '└', Down)  ⇒ true
  }

  private def isRightTurn(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('╮' | '┐', Right) ⇒ true
    case ('╯' | '┘', Down)  ⇒ true
    case ('╭' | '┌', Up)    ⇒ true
    case ('╰' | '└', Left)  ⇒ true
  }

  private def isCrossing(c: Char): Boolean = c == '┼'

}