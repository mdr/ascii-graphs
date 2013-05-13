package com.github.mdr.ascii.layout.drawing

import com.github.mdr.ascii._
import scala.annotation.tailrec
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

class Renderer {

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
    grid(point1) = direction match {
      //      case Up | Down    ⇒ if (grid(point1) == '-') '|' else '|'
      //      case Right | Left ⇒ if (grid(point1) == '|') '|' else '-'
      case Up | Down    ⇒ '|'
      case Right | Left ⇒ '-'
    }
    if (point1 != point2)
      drawLine(grid, point1.go(direction), direction, point2)
  }

  private def render(grid: Grid, element: EdgeDrawingElement) {
    for (segment @ EdgeSegment(point1, direction, point2) ← element.segments) {
      val startPoint =
        if (direction.isVertical && point1 != element.bendPoints.head) point1.go(direction) else point1
      val endPoint =
        if (direction.isVertical && point2 != element.bendPoints.last) point2.go(direction.opposite) else point2

      try
        drawLine(grid, startPoint, direction, endPoint)
      catch {
        case e: Throwable ⇒ throw new RuntimeException("Problem drawing segment " + segment + " in edge " + element, e)
      }
    }
    if (element.hasArrow1)
      for (EdgeSegment(point, direction, _) ← element.segments.headOption)
        grid(point) = direction.opposite.arrow
    if (element.hasArrow2)
      for (EdgeSegment(_, direction, point) ← element.segments.lastOption)
        grid(point) = direction.arrow
  }

  private def render(grid: Grid, element: VertexDrawingElement) {
    val region = element.region

    grid(region.topLeft) = '+'
    grid(region.topRight) = '+'
    grid(region.bottomLeft) = '+'
    grid(region.bottomRight) = '+'

    for (column ← (region.leftColumn + 1) to (region.rightColumn - 1)) {
      grid(Point(region.topRow, column)) = '-'
      grid(Point(region.bottomRow, column)) = '-'
    }
    for (row ← (region.topRow + 1) to (region.bottomRow - 1)) {
      grid(Point(row, region.leftColumn)) = '|'
      grid(Point(row, region.rightColumn)) = '|'
    }

    for ((line, index) ← element.textLines.zipWithIndex)
      grid(region.topLeft.right.down(index + 1)) = line
  }

  def render(drawing: Drawing): String = {
    val grid = new Grid(drawing.dimension)
    for (element ← drawing.elements) element match {
      case vde: VertexDrawingElement ⇒ render(grid, vde)
      case ede: EdgeDrawingElement   ⇒ render(grid, ede)
    }
    grid.toString
  }

}
