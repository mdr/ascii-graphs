package com.github.mdr.ascii.common

object Region {

  def apply(topLeft: Point, dimension: Dimension): Region = {
    val bottomRight = Point(topLeft.row + dimension.height - 1, topLeft.column + dimension.width - 1)
    Region(topLeft, bottomRight)
  }

}

case class Region(topLeft: Point, bottomRight: Point) extends Translatable[Region] {

  def bottomLeft = Point(bottomRight.row, topLeft.column)

  def topRight = Point(topLeft.row, bottomRight.column)

  def topRow = topLeft.row

  def bottomRow = bottomRight.row

  def leftColumn = topLeft.column

  def rightColumn = bottomRight.column

  def expandRight(n: Int) = copy(bottomRight = bottomRight.right(n))

  def expandDown(n: Int) = copy(bottomRight = bottomRight.down(n))

  def expandUp(n: Int) = copy(topLeft = topLeft.up(n))

  def expandLeft(n: Int) = copy(topLeft = topLeft.left(n))

  def contains(point: Point): Boolean = {
    point.row >= topRow && point.column >= leftColumn &&
      point.row <= bottomRow && point.column <= rightColumn
  }

  def contains(region: Region): Boolean = contains(region.topLeft) && contains(region.bottomRight)

  def intersects(region: Region): Boolean = {
    val disjoint =
      this.rightColumn < region.leftColumn ||
        region.rightColumn < this.leftColumn ||
        this.bottomRow < region.topRow ||
        region.bottomRow < this.topRow
    !disjoint
  }

  def width = rightColumn - leftColumn + 1

  def height = bottomRow - topRow + 1

  def dimension = Dimension(height, width)

  def area = width * height

  def points: List[Point] =
    for {
      row ← (topRow to bottomRow toList)
      column ← leftColumn to rightColumn
    } yield Point(row, column)

  def translate(down: Int = 0, right: Int = 0): Region =
    Region(topLeft.translate(down, right), bottomRight.translate(down, right))

  def transpose = Region(topLeft.transpose, bottomRight.transpose)

}
