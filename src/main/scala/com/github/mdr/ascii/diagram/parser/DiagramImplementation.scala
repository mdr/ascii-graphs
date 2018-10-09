package com.github.mdr.ascii.diagram.parser

import com.github.mdr.ascii.diagram._
import com.github.mdr.ascii.common._
import scala.PartialFunction.cond

/**
 * Concrete implementations of Diagram types
 */
trait DiagramImplementation { self: DiagramParser ⇒

  protected class DiagramImpl(numberOfRows: Int, numberOfColumns: Int) extends ContainerImpl with Diagram {

    var allBoxes: List[BoxImpl] = Nil

    var allEdges: List[EdgeImpl] = Nil

    def boxAt(point: Point): Option[BoxImpl] = allBoxes.find(_.boundaryPoints.contains(point))

    def region: Region = diagramRegion

    def contentsRegion = region

  }

  protected case class Label(start: Point, end: Point) {

    require(start.row == end.row)

    val row = start.row

    def points: List[Point] =
      for (column ← (start.column to end.column).toList)
        yield Point(row, column)

    val text: String = {
      val sb = new StringBuilder
      for (column ← start.column + 1 to end.column - 1)
        sb.append(charAt(Point(row, column)))
      sb.toString
    }

  }

  protected abstract class ContainerImpl extends RegionToString { self: Container ⇒

    var text: String = ""

    var childBoxes: List[BoxImpl] = Nil

    def contentsRegion: Region

  }

  protected class BoxImpl(val topLeft: Point, val bottomRight: Point) extends ContainerImpl with Box {

    var edges: List[EdgeImpl] = Nil

    var parent: Option[Container with ContainerImpl] = None

    def region: Region = Region(topLeft, bottomRight)

    def contentsRegion: Region = Region(topLeft.right.down, bottomRight.up.left)

    val leftBoundary: List[Point] = for (row ← (topLeft.row to bottomRight.row).toList) yield Point(row, topLeft.column)
    val rightBoundary: List[Point] = for (row ← (topLeft.row to bottomRight.row).toList) yield Point(row, bottomRight.column)
    val topBoundary: List[Point] = for (column ← (topLeft.column to bottomRight.column).toList) yield Point(topLeft.row, column)
    val bottomBoundary: List[Point] = for (column ← (topLeft.column to bottomRight.column).toList) yield Point(bottomRight.row, column)

    val boundaryPoints: Set[Point] = leftBoundary.toSet ++ rightBoundary.toSet ++ topBoundary.toSet ++ bottomBoundary.toSet

  }

  protected class EdgeImpl(val points: List[Point]) extends Edge {

    val box1: BoxImpl = diagram.boxAt(points.head).get

    val box2: BoxImpl = diagram.boxAt(points.last).get

    var label_ : Option[Label] = None

    lazy val label = label_.map(_.text)

    lazy val parent: Container with ContainerImpl = if (box1.parent == Some(box2)) box2 else box2.parent.get

    lazy val hasArrow1 = isArrow(charAt(points.drop(1).head))

    lazy val hasArrow2 = isArrow(charAt(points.dropRight(1).last))

    lazy val edgeAndLabelPoints: List[Point] = points ++ label_.map(_.points).getOrElse(Nil)

    override def toString = diagramRegionToString(region(edgeAndLabelPoints), edgeAndLabelPoints.contains)

    def region(points: List[Point]): Region =
      Region(
        Point(
          points.map(_.row).min,
          points.map(_.column).min
        ),
        Point(
          points.map(_.row).max,
          points.map(_.column).max
        )
      )

  }

  protected trait RegionToString {

    def region: Region

    override def toString = diagramRegionToString(region)

  }

  private def diagramRegionToString(region: Region, includePoint: Point ⇒ Boolean = p ⇒ true) = {
    val sb = new StringBuilder("\n")
    for (row ← region.topLeft.row to region.bottomRight.row) {
      for {
        column ← region.topLeft.column to region.bottomRight.column
        point = Point(row, column)
        c = if (includePoint(point)) charAt(point) else ' '
      } sb.append(c)
      sb.append("\n")
    }
    sb.toString
  }

  private def isArrow(c: Char) = cond(c) {
    case '^' | '<' | '>' | 'V' | 'v' ⇒ true
  }

}