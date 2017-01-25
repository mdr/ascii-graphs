package com.github.mdr.ascii.layout.drawing

import scala.annotation.tailrec

import com.github.mdr.ascii.common.Direction
import com.github.mdr.ascii.common.Direction._
import com.github.mdr.ascii.common._
import com.github.mdr.ascii.util.Utils

/**
 * An element of a visual depiction of a graph.
 */
sealed trait DrawingElement extends Translatable[DrawingElement] with Transposable[DrawingElement] {

  def translate(down: Int = 0, right: Int = 0): DrawingElement

  def points: List[Point]

  def transpose: DrawingElement

}

/**
 * @param textLines -- each element is a row of text to be rendered inside the vertex.
 */
case class VertexDrawingElement(region: Region, textLines: List[String])
    extends DrawingElement
    with Translatable[VertexDrawingElement]
    with Transposable[VertexDrawingElement]
    with HasRegion {

  //  require(textLines.size <= region.height - 2)
  //  require(textLines.forall(_.length <= region.width - 2))

  def translate(down: Int = 0, right: Int = 0) = copy(region = region.translate(down, right))

  def points = region.points

  def transpose: VertexDrawingElement = copy(region = region.transpose)

}

/**
 * Start and finish points of the edge should not intersect the vertex boxes.
 *
 * @param bendPoints -- points of flex in the edge, includes start (first) and finish (last) points
 * @param hasArrow1 -- has an arrow at the start point.
 * @param hasArrow2 -- has an arrow at the finish point.
 */
case class EdgeDrawingElement(
  bendPoints: List[Point],
  hasArrow1: Boolean,
  hasArrow2: Boolean
)
    extends DrawingElement with Translatable[EdgeDrawingElement] with Transposable[EdgeDrawingElement] {

  lazy val points: List[Point] = segments.flatMap(_.points).distinct

  def translate(down: Int = 0, right: Int = 0) = copy(bendPoints = bendPoints.map(_.translate(down, right)))

  private def direction(point1: Point, point2: Point): Direction = {
    val (Point(r1, c1), Point(r2, c2)) = (point1, point2)
    if (r1 == r2)
      if (c1 < c2)
        Right
      else if (c1 > c2)
        Left
      else
        throw new RuntimeException("Same point: " + point1)
    else if (c1 == c2)
      if (r1 < r2)
        Down
      else if (r1 > r2)
        Up
      else
        throw new RuntimeException("Same point")
    else
      throw new RuntimeException("Points not aligned: " + point1 + ", " + point2)
  }

  lazy val segments: List[EdgeSegment] =
    for ((point1, point2) ← Utils.adjacentPairs(bendPoints))
      yield EdgeSegment(point1, direction(point1, point2), point2)

  def transpose: EdgeDrawingElement = copy(bendPoints = bendPoints.map(_.transpose))

  def replaceSegment(oldSegment: EdgeSegment, newSegment: EdgeSegment): EdgeDrawingElement = {
    val EdgeSegment(newStart, _, newFinish) = newSegment
    val oldIndex = bendPoints.indexOf(oldSegment.start)
    val newBendPoints = bendPoints.patch(oldIndex, List(newStart, newFinish), 2)
    copy(bendPoints = newBendPoints)
  }

  def startPoint = points.head

  def finishPoint = points.last

}

/**
 * A horizontal or vertical segment of an edge. Includes all points in the segment, so overlaps
 * with other segments on the bend points.
 */
case class EdgeSegment(start: Point, direction: Direction, finish: Point) extends HasRegion {

  def points: List[Point] = {
    @tailrec
    def scanForPoints(start: Point, direction: Direction, finish: Point, accum: List[Point]): List[Point] =
      if (start == finish)
        finish :: accum
      else
        scanForPoints(start.go(direction), direction, finish, accum = start :: accum)
    scanForPoints(start, direction, finish, accum = Nil).reverse
  }

  def region = if (start.column < finish.column || start.row < finish.row) Region(start, finish) else Region(finish, start)

}