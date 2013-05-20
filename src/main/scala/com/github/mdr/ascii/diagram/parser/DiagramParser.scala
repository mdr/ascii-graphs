package com.github.mdr.ascii.diagram.parser

import scala.annotation.tailrec
import com.github.mdr.ascii._
import com.github.mdr.ascii.common.Direction._
import com.github.mdr.ascii.common._
import com.github.mdr.ascii.diagram._
import scala.PartialFunction.cond

class DiagramParser(s: String) extends UnicodeEdgeParser with DiagramImplementation with BoxParser with AsciiEdgeParser with LabelParser {

  private val rawRows: List[String] = if (s.isEmpty) Nil else s.split("(\r)?\n").toList

  protected val numberOfColumns = if (rawRows.isEmpty) 0 else rawRows.map(_.length).max

  private val rows = rawRows.map(_.padTo(numberOfColumns, ' ')).toArray

  protected val numberOfRows = rows.length

  protected val diagramRegion = Region(Point(0, 0), Point(numberOfRows - 1, numberOfColumns - 1))

  protected def inDiagram(p: Point): Boolean = diagramRegion contains p

  protected def charAt(point: Point): Char = rows(point.row)(point.column)

  protected def charAtOpt(point: Point): Option[Char] = if (inDiagram(point)) Some(charAt(point)) else None

  protected def isArrow(c: Char) = isDownArrow(c) || isUpArrow(c) || isLeftArrow(c) || isRightArrow(c)

  protected def isDownArrow(c: Char) = c == 'v' || c == 'V'
  protected def isUpArrow(c: Char) = c == '^'
  protected def isLeftArrow(c: Char) = c == '<'
  protected def isRightArrow(c: Char) = c == '>'

  /**
   * @return true iff c is a Unicode Box Drawing character
   */
  protected def isUnicode(c: Char) = c >= 0x2500 && c <= 0x257f

  def getDiagram: Diagram = diagram

  protected val diagram = new DiagramImpl(numberOfRows, numberOfColumns)

  diagram.allBoxes = findAllBoxes

  private val boxContains: Map[BoxImpl, BoxImpl] =
    (for {
      outerBox ← diagram.allBoxes
      innerBox ← diagram.allBoxes
      if outerBox != innerBox
      if outerBox.region contains innerBox.region
    } yield outerBox -> innerBox).toMap

  for {
    (box, containingBoxMap) ← boxContains.groupBy(_._2)
    containingBoxes = box :: containingBoxMap.keys.toList
    orderedBoxes = containingBoxes.sortBy(_.region.area)
    (childBox, parentBox) ← orderedBoxes.zip(orderedBoxes drop 1)
  } {
    childBox.parent = Some(parentBox)
    parentBox.childBoxes ::= childBox
  }

  for (box ← diagram.allBoxes if box.parent.isEmpty) {
    diagram.childBoxes ::= box
    box.parent = Some(diagram)
  }

  protected def isBoxEdge(point: Point) = inDiagram(point) && diagram.allBoxes.exists(_.boundaryPoints.contains(point))

  private def followEdge(direction: Direction, startPoint: Point): Option[EdgeImpl] =
    if (isEdgeStart(charAt(startPoint), direction))
      followUnicodeEdge(startPoint.go(direction) :: startPoint :: Nil, direction)
    else if (!isUnicode(charAt(startPoint)))
      followAsciiEdge(direction, startPoint :: Nil)
    else
      None

  private val edges =
    diagram.allBoxes.flatMap { box ⇒
      box.rightBoundary.flatMap(followEdge(Right, _)) ++
        box.leftBoundary.flatMap(followEdge(Left, _)) ++
        box.topBoundary.flatMap(followEdge(Up, _)) ++
        box.bottomBoundary.flatMap(followEdge(Down, _))
    }

  // Each edge may occur twice (scanned from both ends), so eliminate duplicates:
  diagram.allEdges = edges.groupBy(_.points.toSet).values.toList.map(_.head)

  // Wire up edges to boxes:
  for (edge ← diagram.allEdges) {
    edge.box1.edges ::= edge
    if (edge.box1 != edge.box2)
      edge.box2.edges ::= edge
  }

  protected lazy val allEdgePoints: Set[Point] = diagram.allEdges.flatMap(_.points).toSet

  private lazy val allLabelPoints: Set[Point] =
    (for {
      edge ← diagram.allEdges
      label ← edge.label_.toList
      point ← label.points
    } yield point).toSet

  /**
   * Collect all the text inside a container that isn't part of another diagram element (i.e. a box, edge or label).
   */
  def collectText(container: ContainerImpl): String = {
    val childBoxPoints = container.childBoxes.flatMap(_.region.points).toSet
    val diagramPoints = childBoxPoints ++ allEdgePoints ++ allLabelPoints

    val region = container.contentsRegion
    val sb = new StringBuilder
    for (row ← region.topLeft.row to region.bottomRight.row) {
      for {
        column ← region.topLeft.column to region.bottomRight.column
        point = Point(row, column)
        if !diagramPoints.contains(point)
        c = charAt(point)
      } sb.append(c)
      sb.append("\n")
    }
    if (sb.nonEmpty)
      sb.deleteCharAt(sb.length - 1)
    sb.toString
  }

  for (edge ← diagram.allEdges)
    edge.label_ = getLabel(edge)

  for (box ← diagram.allBoxes)
    box.text = collectText(box)
  diagram.text = collectText(diagram)

}