package com.github.mdr.ascii.layout.drawing

import scala.annotation.tailrec
import scala.PartialFunction.condOpt

import com.github.mdr.ascii.parser._
import com.github.mdr.ascii.util.Utils

object Renderer {

  def render(drawing: Drawing) = new Renderer().render(drawing)

}

class Renderer(unicode: Boolean = true, doubleVertices: Boolean = false, rounded: Boolean = true) {

  @tailrec
  private def drawLine(grid: Grid, point1: Point, direction: Direction, point2: Point) {
    val lineChar = direction match {
      case Up | Down    ⇒ lineHorizontalChar
      case Right | Left ⇒ lineVerticalChar
    }
    grid(point1) = if (grid(point1) == backgroundChar) lineChar else intersectionCharOpt.getOrElse(lineChar)
    if (point1 != point2)
      drawLine(grid, point1.go(direction), direction, point2)
  }

  private def render(grid: Grid, element: EdgeDrawingElement, drawing: Drawing) {
    for ((previousSegmentOpt, segment @ EdgeSegment(point1, direction, point2)) ← Utils.withPrevious(element.segments)) {
      val startPoint =
        if (direction.isVertical && point1 != element.bendPoints.head) point1.go(direction) else point1
      val endPoint =
        if (direction.isVertical && point2 != element.bendPoints.last) point2.go(direction.opposite) else point2

      try
        drawLine(grid, startPoint, direction, endPoint)
      catch {
        case e: Throwable ⇒ throw new RuntimeException("Problem drawing segment " + segment + " in edge " + element, e)
      }

      if (unicode)
        condOpt(previousSegmentOpt.map(_.direction), direction) {
          case (Some(Up), Right) | (Some(Left), Down) ⇒ grid(point1) = bendChar1
          case (Some(Up), Left) | (Some(Right), Down) ⇒ grid(point1) = bendChar2
          case (Some(Down), Right) | (Some(Left), Up) ⇒ grid(point1) = bendChar3
          case (Some(Down), Left) | (Some(Right), Up) ⇒ grid(point1) = bendChar4
        }
    }

    def drawBoxIntersection(intersectionPoint: Point, direction: Direction) =
      if (unicode && drawing.vertexElementAt(intersectionPoint).isDefined && grid.contains(intersectionPoint))
        grid(intersectionPoint) = direction match {
          case Up    ⇒ joinChar1
          case Down  ⇒ joinChar2
          case Right ⇒ joinChar3
          case Left  ⇒ joinChar4
        }

    for (EdgeSegment(point, direction, _) ← element.segments.headOption)
      if (element.hasArrow1)
        grid(point) = direction.opposite.arrow
      else
        drawBoxIntersection(point.go(direction.opposite), direction.opposite)

    for (EdgeSegment(_, direction, point) ← element.segments.lastOption)
      if (element.hasArrow2)
        grid(point) = direction.arrow
      else
        drawBoxIntersection(point.go(direction), direction)

  }

  private def render(grid: Grid, element: VertexDrawingElement) {
    val region = element.region

    grid(region.topLeft) = topLeftChar
    grid(region.topRight) = topRightChar
    grid(region.bottomLeft) = bottomLeftChar
    grid(region.bottomRight) = bottomRightChar

    for (column ← (region.leftColumn + 1) to (region.rightColumn - 1)) {
      grid(Point(region.topRow, column)) = boxHorizontalChar
      grid(Point(region.bottomRow, column)) = boxHorizontalChar
    }
    for (row ← (region.topRow + 1) to (region.bottomRow - 1)) {
      grid(Point(row, region.leftColumn)) = boxVerticalChar
      grid(Point(row, region.rightColumn)) = boxVerticalChar
    }

    for ((line, index) ← element.textLines.zipWithIndex)
      grid(region.topLeft.right.down(index + 1)) = line
  }

  def render(drawing: Drawing): String = {
    val grid = new Grid(drawing.dimension)
    drawing.vertexElements.foreach(vde ⇒ render(grid, vde))
    drawing.edgeElements.foreach(ede ⇒ render(grid, ede, drawing))
    grid.toString
  }

  private def lineHorizontalChar = if (unicode) '│' else '|'
  private def lineVerticalChar = if (unicode) '─' else '-'

  private def bendChar1 = if (rounded) '╭' else '┌'
  private def bendChar2 = if (rounded) '╮' else '┐'
  private def bendChar3 = if (rounded) '╰' else '└'
  private def bendChar4 = if (rounded) '╯' else '┘'

  private def intersectionCharOpt = if (unicode) Some('┼') else None

  private def topLeftChar = if (doubleVertices) '╔' else if (rounded) '╭' else if (unicode) '┌' else '+'
  private def topRightChar = if (doubleVertices) '╗' else if (rounded) '╮' else if (unicode) '┐' else '+'
  private def bottomLeftChar = if (doubleVertices) '╚' else if (rounded) '╰' else if (unicode) '└' else '+'
  private def bottomRightChar = if (doubleVertices) '╝' else if (rounded) '╯' else if (unicode) '┘' else '+'

  private def boxHorizontalChar = if (doubleVertices) '═' else if (unicode) '─' else '-'
  private def boxVerticalChar = if (doubleVertices) '║' else if (unicode) '│' else '|'

  private def joinChar1 = if (doubleVertices) '╤' else '┬'
  private def joinChar2 = if (doubleVertices) '╧' else '┴'
  private def joinChar3 = if (doubleVertices) '╢' else '┤'
  private def joinChar4 = if (doubleVertices) '╟' else '├'

  private def backgroundChar = ' '

}

