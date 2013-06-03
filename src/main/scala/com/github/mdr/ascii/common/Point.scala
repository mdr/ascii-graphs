package com.github.mdr.ascii.common

object Point {

  private def sameColumn(p1: Point, p2: Point, p3: Point) = p1.column == p2.column && p2.column == p3.column
  private def sameRow(p1: Point, p2: Point, p3: Point) = p1.row == p2.row && p2.row == p3.row
  private def colinear(p1: Point, p2: Point, p3: Point) = sameColumn(p1, p2, p3) || sameRow(p1, p2, p3)

  def removeRedundantPoints(points: List[Point]): List[Point] = points match {
    case List() | List(_) | List(_, _)                       ⇒ points
    case p1 :: p2 :: p3 :: remainder if colinear(p1, p2, p3) ⇒ removeRedundantPoints(p1 :: p3 :: remainder)
    case p :: ps                                             ⇒ p :: removeRedundantPoints(ps)
  }

}

case class Point(row: Int, column: Int) extends Translatable[Point] with Transposable[Point] {

  def maxRowCol(that: Point): Point = Point(math.max(this.row, that.row), math.max(this.column, that.column))

  type Self = Point

  def translate(down: Int = 0, right: Int = 0): Point = Point(row + down, column + right)

  def transpose: Point = Point(column, row)

  def neighbours: List[Point] = List(up, right, down, left)

  def withRow(newRow: Int) = copy(row = newRow)

  def withColumn(newColumn: Int) = copy(column = newColumn)

  def region: Region = Region(this, this)

}
