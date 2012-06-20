package com.github.mdr.ascii.layout

import com.github.mdr.ascii._
import scala.annotation.tailrec

sealed trait DrawingElement {

  def translate(down: Int = 0, right: Int = 0): DrawingElement

}

case class VertexDrawingElement(region: Region, textLines: List[String]) extends DrawingElement {

  def translate(down: Int = 0, right: Int = 0) = copy(region = region.translate(down, right))

}

case class EdgeDrawingElement(
  points: List[Point],
  hasArrow1: Boolean,
  hasArrow2: Boolean)
  extends DrawingElement {

  def translate(down: Int = 0, right: Int = 0) = copy(points = points.map(_.translate(down, right)))

  private def direction(point1: Point, point2: Point): Direction =
    if (point1.row == point2.row) {
      if (point1.column < point2.column)
        Right
      else if (point1.column > point2.column)
        Left
      else
        throw new RuntimeException("Same point")
    } else if (point1.column == point2.column) {
      if (point1.row < point2.row)
        Down
      else if (point1.row > point2.row)
        Up
      else
        throw new RuntimeException("Same point")
    } else
      throw new RuntimeException("Points not aligned: " + point1 + ", " + point2)

  lazy val pointAndDirections: List[(Point, Direction, Point)] =
    for ((point1, point2) ← points.zip(points.drop(1)))
      yield (point1, direction(point1, point2), point2)

}

object Renderer {

  def render(elements: List[DrawingElement]) = new Renderer().render(elements)

}

class Renderer {

  private def combinePoints(point1: Point, point2: Point) =
    Point(math.max(point1.row, point2.row), math.max(point1.column, point2.column))

  class Grid(width: Int, height: Int) {

    private def makeRow: Array[Char] = (" " * width).toArray
    val chars = (1 to height).map(r ⇒ makeRow).toArray

    def apply(point: Point): Char = chars(point.row)(point.column)

    def update(point: Point, char: Char) {
      chars(point.row)(point.column) = char
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
      case Up | Down    ⇒ if (grid(point1) == '-') '|' else '|'
      case Right | Left ⇒ if (grid(point1) == '|') '|' else '-'
      //      case Up | Down    ⇒ if (grid(point1) == '-') '+' else '|'
      //      case Right | Left ⇒ if (grid(point1) == '|') '+' else '-'
    }
    if (point1 != point2)
      drawLine(grid, point1.go(direction), direction, point2)
  }

  private def render(grid: Grid, element: EdgeDrawingElement) {
    for ((point1, direction, point2) ← element.pointAndDirections) {
      val startPoint =
        if (direction.isVertical && point1 != element.points.head) point1.go(direction) else point1
      val endPoint =
        if (direction.isVertical && point2 != element.points.last) point2.go(direction.opposite) else point2

      drawLine(grid, startPoint, direction, endPoint)
    }
    if (element.hasArrow1)
      for ((point, direction, _) ← element.pointAndDirections.headOption)
        grid(point) = direction.opposite.arrow
    if (element.hasArrow2)
      for ((_, direction, point) ← element.pointAndDirections.lastOption)
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

  def render(elements: List[DrawingElement]): String = {
    val largestPoint =
      elements.flatMap {
        case element: VertexDrawingElement ⇒ List(element.region.bottomRight)
        case element: EdgeDrawingElement   ⇒ element.points
      }.reduceLeft(combinePoints)
    val grid = new Grid(width = largestPoint.column + 1, height = largestPoint.row + 1)

    for (element ← elements) element match {
      case vde: VertexDrawingElement ⇒ render(grid, vde)
      case ede: EdgeDrawingElement   ⇒ render(grid, ede)
    }
    grid.toString
  }

}