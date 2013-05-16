package com.github.mdr.ascii.layout.drawing

import com.github.mdr.ascii.common.Point

/**
 * Mutable 2D representation of whether or not positions are occupied.
 */
class OccupancyGrid(drawing: Drawing) {

  /**
   * For each position, record the number of elements that contribute to that position being occupied
   */
  private val grid: Array[Array[Int]] = Array.fill(drawing.dimension.height, drawing.dimension.width)(0)

  {
    drawing.elements.foreach(add)
  }

  def apply(point: Point): Boolean = grid(point.row)(point.column) > 0

  def isOccupied(point: Point) = this(point)

  private def add(element: DrawingElement) = adjust(element, 1)

  private def remove(element: DrawingElement) = adjust(element, -1)

  def replace(element1: DrawingElement, element2: DrawingElement) {
    remove(element1)
    add(element2)
  }

  private def adjust(drawingElement: DrawingElement, delta: Int) =
    for {
      point ← drawingElement.points
    } grid(point.row)(point.column) += delta

  override def toString = {
    def renderRow(row: Array[Int]) = row.map(n ⇒ if (n == 0) ' ' else n.toString).mkString
    grid.map(renderRow).mkString("\n")
  }

}