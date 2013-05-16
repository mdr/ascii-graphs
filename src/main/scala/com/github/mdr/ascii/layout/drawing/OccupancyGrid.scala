package com.github.mdr.ascii.layout.drawing

import com.github.mdr.ascii.common.Point

class OccupancyGrid(drawing: Drawing) {

  private val grid: Array[Array[Boolean]] = Array.fill(drawing.dimension.height, drawing.dimension.width)(false)

  drawing.elements.foreach(record)

  def apply(point: Point): Boolean = grid(point.row)(point.column)

  def isOccupied(point: Point) = this(point)

  private def record(drawingElement: DrawingElement) = drawingElement.points.foreach(markAsOccupied)

  private def markAsOccupied(point: Point) {
    grid(point.row)(point.column) = true
  }

}