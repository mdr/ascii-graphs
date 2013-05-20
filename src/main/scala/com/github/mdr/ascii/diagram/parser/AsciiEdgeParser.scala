package com.github.mdr.ascii.diagram.parser

import scala.annotation.tailrec
import com.github.mdr.ascii.common.Point
import com.github.mdr.ascii.common.Direction
import com.github.mdr.ascii.common.Direction._

trait AsciiEdgeParser { self: DiagramParser ⇒

  @tailrec
  protected final def followAsciiEdge(direction: Direction, edgeSoFar: List[Point]): Option[EdgeImpl] = {
    val currentPoint = edgeSoFar.head
    if (!inDiagram(currentPoint))
      return None
    val ahead: Point = currentPoint.go(direction)

    def mkMatcher(pred: Char ⇒ Boolean) = new {
      def unapply(cOpt: Option[Char]) = cOpt.flatMap { c ⇒ if (pred(c)) Some(c) else None }
    }

    val UpArrow = mkMatcher(isUpArrow)
    val DownArrow = mkMatcher(isDownArrow)
    val LeftArrow = mkMatcher(isLeftArrow)
    val RightArrow = mkMatcher(isRightArrow)
    val EdgeCrossing = mkMatcher(isEdgeCrossing)
    val EdgeChar = mkMatcher(isEdge)
    val Horizontal = mkMatcher(isHorizontalEdge)
    val Vertical = mkMatcher(isVerticalEdge)
    val HorizOrVert = mkMatcher(c ⇒ isVerticalEdge(c) || isHorizontalEdge(c))
    val VertOrCrossing = mkMatcher(c ⇒ isVerticalEdge(c) || isEdgeCrossing(c))
    val HorizOrCrossing = mkMatcher(c ⇒ isHorizontalEdge(c) || isEdgeCrossing(c))

    val RightToDown = mkMatcher(isRightToDownEdgeBend)
    val RightToUp = mkMatcher(isRightToUpEdgeBend)
    val LeftToDown = mkMatcher(isLeftToDownEdgeBend)
    val LeftToUp = mkMatcher(isLeftToUpEdgeBend)

    if (direction.isHorizontal)
      (charAtOpt(ahead), charAtOpt(ahead.go(Up)), charAtOpt(ahead.go(Down)), charAtOpt(ahead.go(direction))) match {
        case _ if isBoxEdge(ahead)                                     ⇒ finaliseEdge(ahead, edgeSoFar)
        case (EdgeCrossing(_), _, _, _)                                ⇒ followAsciiEdge(direction, ahead :: edgeSoFar)
        case (DownArrow(_), _, _, _)                                   ⇒ followAsciiEdge(Down, ahead :: edgeSoFar)
        case (UpArrow(_), _, _, _)                                     ⇒ followAsciiEdge(Up, ahead :: edgeSoFar)
        case (Horizontal(_), _, _, EdgeChar(_))                        ⇒ followAsciiEdge(direction, ahead :: edgeSoFar)
        case (RightToUp(_), _, _, _) if direction == Right             ⇒ followAsciiEdge(Up, ahead :: edgeSoFar)
        case (RightToDown(_), _, _, _) if direction == Right           ⇒ followAsciiEdge(Down, ahead :: edgeSoFar)
        case (LeftToUp(_), _, _, _) if direction == Left               ⇒ followAsciiEdge(Up, ahead :: edgeSoFar)
        case (LeftToDown(_), _, _, _) if direction == Left             ⇒ followAsciiEdge(Down, ahead :: edgeSoFar)
        case (HorizOrVert(_), UpArrow(_), DownArrow(_), _)             ⇒ None
        case (HorizOrVert(_), UpArrow(_), _, _)                        ⇒ followAsciiEdge(Up, ahead :: edgeSoFar)
        case (HorizOrVert(_), _, DownArrow(_), _)                      ⇒ followAsciiEdge(Down, ahead :: edgeSoFar)
        case (HorizOrVert(_), VertOrCrossing(_), VertOrCrossing(_), _) ⇒ None
        case (HorizOrVert(_), VertOrCrossing(_), _, _)                 ⇒ followAsciiEdge(Up, ahead :: edgeSoFar)
        case (HorizOrVert(_), _, VertOrCrossing(_), _)                 ⇒ followAsciiEdge(Down, ahead :: edgeSoFar)
        case (Vertical(_), Horizontal(_), Horizontal(_), _)            ⇒ None
        case (Vertical(_), Horizontal(_), _, _)                        ⇒ followAsciiEdge(Up, ahead :: edgeSoFar)
        case (Vertical(_), _, Horizontal(_), _)                        ⇒ followAsciiEdge(Down, ahead :: edgeSoFar)
        case (LeftArrow(_), _, _, _) if direction == Left              ⇒ followAsciiEdge(direction, ahead :: edgeSoFar)
        case (RightArrow(_), _, _, _) if direction == Right            ⇒ followAsciiEdge(direction, ahead :: edgeSoFar)
        case _                                                         ⇒ None
      }
    else
      (charAtOpt(ahead), charAtOpt(ahead.go(Left)), charAtOpt(ahead.go(Right)), charAtOpt(ahead.go(direction))) match {
        case _ if isBoxEdge(ahead)                                       ⇒ finaliseEdge(ahead, edgeSoFar)
        case (EdgeCrossing(_), _, _, _)                                  ⇒ followAsciiEdge(direction, ahead :: edgeSoFar)
        case (LeftArrow(_), _, _, _)                                     ⇒ followAsciiEdge(Left, ahead :: edgeSoFar)
        case (RightArrow(_), _, _, _)                                    ⇒ followAsciiEdge(Right, ahead :: edgeSoFar)
        case (Vertical(_), _, _, EdgeChar(_))                            ⇒ followAsciiEdge(direction, ahead :: edgeSoFar)
        case (RightToUp(_), _, _, _) if direction == Down                ⇒ followAsciiEdge(Left, ahead :: edgeSoFar)
        case (RightToDown(_), _, _, _) if direction == Up                ⇒ followAsciiEdge(Left, ahead :: edgeSoFar)
        case (LeftToUp(_), _, _, _) if direction == Down                 ⇒ followAsciiEdge(Right, ahead :: edgeSoFar)
        case (LeftToDown(_), _, _, _) if direction == Up                 ⇒ followAsciiEdge(Right, ahead :: edgeSoFar)
        case (HorizOrVert(_), LeftArrow(_), RightArrow(_), _)            ⇒ None
        case (HorizOrVert(_), LeftArrow(_), _, _)                        ⇒ followAsciiEdge(Left, ahead :: edgeSoFar)
        case (HorizOrVert(_), _, RightArrow(_), _)                       ⇒ followAsciiEdge(Right, ahead :: edgeSoFar)
        case (HorizOrVert(_), HorizOrCrossing(_), HorizOrCrossing(_), _) ⇒ None
        case (HorizOrVert(_), HorizOrCrossing(_), _, _)                  ⇒ followAsciiEdge(Left, ahead :: edgeSoFar)
        case (HorizOrVert(_), _, HorizOrCrossing(_), _)                  ⇒ followAsciiEdge(Right, ahead :: edgeSoFar)
        case (Horizontal(_), Vertical(_), Vertical(_), _)                ⇒ None
        case (Horizontal(_), Vertical(_), _, _)                          ⇒ followAsciiEdge(Left, ahead :: edgeSoFar)
        case (Horizontal(_), _, Vertical(_), _)                          ⇒ followAsciiEdge(Right, ahead :: edgeSoFar)
        case (UpArrow(_), _, _, _) if direction == Up                    ⇒ followAsciiEdge(direction, ahead :: edgeSoFar)
        case (DownArrow(_), _, _, _) if direction == Down                ⇒ followAsciiEdge(direction, ahead :: edgeSoFar)
        case _                                                           ⇒ None
      }
  }

  private def finaliseEdge(connectPoint: Point, edgeSoFar: List[Point]): Option[EdgeImpl] = {
    val points = (connectPoint :: edgeSoFar).reverse
    if (points.size <= 2)
      None
    else
      Some(new EdgeImpl(points))
  }

  private def isEdgeCrossing(c: Char): Boolean = c == '+' || c == '┼'
  private def isEdge(c: Char) =
    isArrow(c) || isEdgeCrossing(c) || isHorizontalEdge(c) || isVerticalEdge(c) || isBendChar(c)
  private def isHorizontalEdge(c: Char) = c == '─' || c == '-'
  private def isVerticalEdge(c: Char) = c == '│' || c == '|'

  private def isRightToDownEdgeBend(c: Char) = c == '╮' || c == '┐'
  private def isRightToUpEdgeBend(c: Char) = c == '╯' || c == '┘'
  private def isLeftToDownEdgeBend(c: Char) = c == '╭' | c == '┌'
  private def isLeftToUpEdgeBend(c: Char) = c == '╰' || c == '└'

  private def isBendChar(c: Char) =
    isRightToDownEdgeBend(c) || isRightToUpEdgeBend(c) || isLeftToDownEdgeBend(c) || isLeftToUpEdgeBend(c)

}