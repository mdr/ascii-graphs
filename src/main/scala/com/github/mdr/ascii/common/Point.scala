package com.github.mdr.ascii.common

case class Point(row: Int, column: Int) extends Translatable[Point] {

  def maxRowCol(that: Point): Point = Point(math.max(this.row, that.row), math.max(this.column, that.column))

  type Self = Point

  def translate(down: Int = 0, right: Int = 0): Point = Point(row + down, column + right)

  def transpose: Point = Point(column, row)

  def neighbours: List[Point] = List(up, right, down, left)

}
