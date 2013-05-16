package com.github.mdr.ascii.common

object Dimension {

  def fromPoint(largestPoint: Point): Dimension =
    Dimension(width = largestPoint.column + 1, height = largestPoint.row + 1)

}

case class Dimension(height: Int, width: Int) extends Transposable[Dimension] {

  def transpose = Dimension(width, height)

}
