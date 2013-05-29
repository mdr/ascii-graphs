package com.github.mdr.ascii.layout.drawing

import scala.annotation.tailrec

import com.github.mdr.ascii.common.Direction
import com.github.mdr.ascii.common.Direction._
import com.github.mdr.ascii.common._
import com.github.mdr.ascii.util.Utils

/**
 * A visual artifact in a rendered graph.
 */
sealed trait DrawingElement extends Translatable[DrawingElement] with Transposable[DrawingElement] {

  def translate(down: Int = 0, right: Int = 0): DrawingElement

  def points: List[Point]

  def transpose: DrawingElement

}

/**
 * Visual rendering of a graph vertex.
 */
case class VertexDrawingElement(region: Region, textLines: List[String])
    extends DrawingElement
    with Translatable[VertexDrawingElement]
    with Transposable[VertexDrawingElement] {

  def translate(down: Int = 0, right: Int = 0) = copy(region = region.translate(down, right))

  def points = region.points

  def transpose: VertexDrawingElement = copy(region = region.transpose)

}

/**
 * Visual rendering of a directed edge.
 *
 * Start and finish points of the edge should not intersect the vertex boxes.
 *
 * @param bendPoints -- points of flex in the edge, includes start and finish points
 */
case class EdgeDrawingElement(
  bendPoints: List[Point],
  hasArrow1: Boolean,
  hasArrow2: Boolean)
    extends DrawingElement with Translatable[EdgeDrawingElement] with Transposable[EdgeDrawingElement] {

  lazy val points: List[Point] = segments.flatMap(_.points).distinct

  def translate(down: Int = 0, right: Int = 0) = copy(bendPoints = bendPoints.map(_.translate(down, right)))

  private def direction(point1: Point, point2: Point): Direction =
    if (point1.row == point2.row) {
      if (point1.column < point2.column)
        Right
      else if (point1.column > point2.column)
        Left
      else
        throw new RuntimeException("Same point: " + point1)
    } else if (point1.column == point2.column) {
      if (point1.row < point2.row)
        Down
      else if (point1.row > point2.row)
        Up
      else
        throw new RuntimeException("Same point")
    } else
      throw new RuntimeException("Points not aligned: " + point1 + ", " + point2)

  lazy val segments: List[EdgeSegment] =
    for ((point1, point2) ← Utils.adjacentPairs(bendPoints))
      yield EdgeSegment(point1, direction(point1, point2), point2)

  def transpose: EdgeDrawingElement = copy(bendPoints = bendPoints.map(_.transpose))

}

case class EdgeSegment(start: Point, direction: Direction, finish: Point) {

  def points: List[Point] = {
    @tailrec
    def scanForPoints(start: Point, direction: Direction, finish: Point, accum: List[Point]): List[Point] =
      if (start == finish)
        finish :: accum
      else
        scanForPoints(start.go(direction), direction, finish, accum = start :: accum)
    scanForPoints(start, direction, finish, accum = Nil).reverse
  }

}
