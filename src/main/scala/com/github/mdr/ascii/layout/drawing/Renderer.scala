package com.github.mdr.ascii.layout.drawing

import com.github.mdr.ascii._
import scala.annotation.tailrec
import scala.PartialFunction.condOpt
import com.github.mdr.ascii.util.Utils
import com.github.mdr.ascii.parser.Up
import com.github.mdr.ascii.parser.Translatable
import com.github.mdr.ascii.parser.Right
import com.github.mdr.ascii.parser.Region
import com.github.mdr.ascii.parser.Point
import com.github.mdr.ascii.parser.Left
import com.github.mdr.ascii.parser.Down
import com.github.mdr.ascii.parser.Direction
import com.github.mdr.ascii.parser.Dimension

object Renderer {

  def render(drawing: Drawing) = new Renderer().render(drawing)

}

class Renderer(unicode: Boolean = true, doubleVertices: Boolean = false, rounded: Boolean = true) {

  class Grid(dimension: Dimension) {

    val chars: Array[Array[Char]] = Array.fill(dimension.height, dimension.width)(' ')

    def apply(point: Point): Char = chars(point.row)(point.column)

    def update(point: Point, char: Char) {
      val row = chars(point.row)
      row(point.column) = char
    }

    def update(point: Point, s: String) {
      var p = point
      for (c ← s) {
        this(p) = c
        p = p.right
      }
    }

    override def toString = chars.map(new String(_)).mkString("\n")
  }

  @tailrec
  private def drawLine(grid: Grid, point1: Point, direction: Direction, point2: Point) {
    if (unicode && grid(point1) != ' ')
      grid(point1) = '┼'
    else
      grid(point1) = direction match {
        case Up | Down    ⇒ if (unicode) '│' else '|'
        case Right | Left ⇒ if (unicode) '─' else '-'
      }
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
          case (Some(Up), Right) | (Some(Left), Down) ⇒ grid(point1) = if (rounded) '╭' else '┌'
          case (Some(Up), Left) | (Some(Right), Down) ⇒ grid(point1) = if (rounded) '╮' else '┐'
          case (Some(Down), Right) | (Some(Left), Up) ⇒ grid(point1) = if (rounded) '╰' else '└'
          case (Some(Down), Left) | (Some(Right), Up) ⇒ grid(point1) = if (rounded) '╯' else '┘'
        }

    }
    for (EdgeSegment(point, direction, _) ← element.segments.headOption)
      if (element.hasArrow1)
        grid(point) = direction.opposite.arrow
      else if (unicode) {
        val afterPoint = point.go(direction.opposite)
        if (drawing.vertexElementAt(afterPoint).isDefined) {
          grid(afterPoint) = direction.opposite match {
            case Up    ⇒ if (doubleVertices) '╤' else '┬'
            case Down  ⇒ if (doubleVertices) '╧' else '┴'
            case Right ⇒ if (doubleVertices) '╢' else '┤'
            case Left  ⇒ if (doubleVertices) '╟' else '├'
          }
        }
      }
    for (EdgeSegment(_, direction, point) ← element.segments.lastOption)
      if (element.hasArrow2)
        grid(point) = direction.arrow
      else if (unicode) {
        val afterPoint = point.go(direction)
        if (drawing.vertexElementAt(afterPoint).isDefined) {
          grid(afterPoint) = direction match {
            case Up    ⇒ if (doubleVertices) '╤' else '┬'
            case Down  ⇒ if (doubleVertices) '╧' else '┴'
            case Right ⇒ if (doubleVertices) '╢' else '┤'
            case Left  ⇒ if (doubleVertices) '╟' else '├'
          }
        }
      }

  }

  private def render(grid: Grid, element: VertexDrawingElement) {
    val region = element.region

    grid(region.topLeft) = if (doubleVertices) '╔' else if (rounded) '╭' else if (unicode) '┌' else '+'
    grid(region.topRight) = if (doubleVertices) '╗' else if (rounded) '╮' else if (unicode) '┐' else '+'
    grid(region.bottomLeft) = if (doubleVertices) '╚' else if (rounded) '╰' else if (unicode) '└' else '+'
    grid(region.bottomRight) = if (doubleVertices) '╝' else if (rounded) '╯' else if (unicode) '┘' else '+'

    for (column ← (region.leftColumn + 1) to (region.rightColumn - 1)) {
      grid(Point(region.topRow, column)) = if (doubleVertices) '═' else if (unicode) '─' else '-'
      grid(Point(region.bottomRow, column)) = if (doubleVertices) '═' else if (unicode) '─' else '-'
    }
    for (row ← (region.topRow + 1) to (region.bottomRow - 1)) {
      grid(Point(row, region.leftColumn)) = if (doubleVertices) '║' else if (unicode) '│' else '|'
      grid(Point(row, region.rightColumn)) = if (doubleVertices) '║' else if (unicode) '│' else '|'
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

}
