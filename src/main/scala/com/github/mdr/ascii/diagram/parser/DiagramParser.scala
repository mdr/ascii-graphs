package com.github.mdr.ascii.diagram.parser

import scala.annotation.tailrec
import com.github.mdr.ascii._
import com.github.mdr.ascii.common.Direction._
import com.github.mdr.ascii.common._
import com.github.mdr.ascii.diagram._
import scala.PartialFunction.cond

class DiagramParser(s: String) {

  private val rawRows: List[String] = if (s.isEmpty) Nil else s.split("(\r)?\n").toList

  private val numberOfColumns = if (rawRows.isEmpty) 0 else rawRows.map(_.length).max

  private val rows = rawRows.map(_.padTo(numberOfColumns, ' ')).toArray

  private val numberOfRows = rows.length

  private val diagramRegion = Region(Point(0, 0), Point(numberOfRows - 1, numberOfColumns - 1))

  private def inDiagram(p: Point): Boolean = diagramRegion contains p

  private def charAt(point: Point): Char = rows(point.row)(point.column)

  private def charAtOpt(point: Point): Option[Char] = if (inDiagram(point)) Some(charAt(point)) else None

  private def isTopRightCorner(c: Char): Boolean = cond(c) { case '╗' | '╮' | '┐' | '+' ⇒ true }
  private def isBottomRightCorner(c: Char): Boolean = cond(c) { case '╝' | '╯' | '┘' | '+' ⇒ true }
  private def isTopLeftCorner(c: Char): Boolean = cond(c) { case '╔' | '╭' | '┌' | '+' ⇒ true }
  private def isBottomLeftCorner(c: Char): Boolean = cond(c) { case '╚' | '╰' | '└' | '+' ⇒ true }
  private def isHorizontalBoxEdge(c: Char): Boolean = cond(c) {
    case '═' | '─' | '-' | '╤' | '┬' | '╧' | '┴' | '╪' | '┼' ⇒ true
  }
  private def isVerticalBoxEdge(c: Char): Boolean = cond(c) {
    case '║' | '│' | '|' | '╢' | '┤' | '╟' | '├' | '╫' | '┼' ⇒ true
  }
  private def isArrow(c: Char) = isDownArrow(c) || isUpArrow(c) || isLeftArrow(c) || isRightArrow(c)
  private def isDownArrow(c: Char) = c == 'v' || c == 'V'
  private def isUpArrow(c: Char) = c == '^'
  private def isLeftArrow(c: Char) = c == '<'
  private def isRightArrow(c: Char) = c == '>'
  private def isEdgeCrossing(c: Char): Boolean = c == '+' || c == '┼'
  private def isEdge(c: Char) =
    isArrow(c) || isEdgeCrossing(c) || isHorizontalEdge(c) || isVerticalEdge(c) || isBendChar(c)
  private def isHorizontalEdge(c: Char) = c == '─' || c == '-'
  private def isVerticalEdge(c: Char) = c == '│' || c == '|'

  private def isRightToDownEdgeBend(c: Char) = c == '╮' || c == '┐'
  private def isRightToUpEdgeBend(c: Char) = c == '╯' || c == '┘'
  private def isLeftToDownEdgeBend(c: Char) = c == '╭' | c == '┌'
  private def isLeftToUpEdgeBend(c: Char) = c == '╰' || c == '└'
  private def isBendChar(c: Char) =
    isRightToDownEdgeBend(c) || isRightToUpEdgeBend(c) || isLeftToDownEdgeBend(c) || isLeftToUpEdgeBend(c)

  @tailrec
  private def scanBoxEdge(p: Point, dir: Direction, isCorner: Char ⇒ Boolean, isEdge: Char ⇒ Boolean): Option[Point] =
    if (inDiagram(p)) {
      val c = charAt(p)
      if (isCorner(c))
        Some(p)
      else if (isEdge(c))
        scanBoxEdge(p.go(dir), dir, isCorner, isEdge)
      else
        None
    } else
      None

  /**
   * @return bottomRight of box if all edges are correct
   */
  private def completeBox(topLeft: Point): Option[Point] =
    for {
      topRight ← scanBoxEdge(topLeft.right, Right, isTopRightCorner, isHorizontalBoxEdge)
      bottomRight ← scanBoxEdge(topRight.down, Down, isBottomRightCorner, isVerticalBoxEdge)
      bottomLeft ← scanBoxEdge(topLeft.down, Down, isBottomLeftCorner, isVerticalBoxEdge)
      bottomRight2 ← scanBoxEdge(bottomLeft.right, Right, isBottomRightCorner, isHorizontalBoxEdge)
      if bottomRight == bottomRight2
    } yield bottomRight

  private val possibleTopLefts: List[Point] =
    for {
      row ← (0 until numberOfRows - 1).toList
      column ← 0 until numberOfColumns - 1
      point = Point(row, column)
      if isTopLeftCorner(charAt(point))
      if isUnicode(charAt(point.right)) || isHorizontalBoxEdge(charAt(point.right)) // Unicode chars don't require even a single right / left box char
      if isUnicode(charAt(point.down)) || isVerticalBoxEdge(charAt(point.down))
    } yield point

  private def isUnicode(c: Char) = c > 1000 // TODO

  private val allBoxes =
    for {
      topLeft ← possibleTopLefts
      bottomRight ← completeBox(topLeft)
    } yield new BoxImpl(topLeft, bottomRight)

  def getDiagram: Diagram = diagram

  private val diagram = new DiagramImpl(numberOfRows, numberOfColumns)

  diagram.allBoxes = allBoxes

  private val boxContains: Map[BoxImpl, BoxImpl] =
    (for {
      outerBox ← allBoxes
      innerBox ← allBoxes
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

  for (box ← allBoxes if box.parent.isEmpty) {
    diagram.childBoxes ::= box
    box.parent = Some(diagram)
  }

  private def isBoxEdge(point: Point) = inDiagram(point) && allBoxes.exists(_.boundaryPoints.contains(point))

  @tailrec
  private def followEdge(direction: Direction, edgeSoFar: List[Point]): Option[EdgeImpl] = {
    val currentPoint = edgeSoFar.head
    if (!inDiagram(currentPoint))
      return None

    def finaliseEdge(connectPoint: Point): Option[EdgeImpl] = {
      val points = (connectPoint :: edgeSoFar).reverse
      if (points.size <= 2)
        None
      else
        Some(new EdgeImpl(points))
    }
    val ahead: Point = currentPoint.go(direction)

    def mkMatcher(pred: Char ⇒ Boolean) = new {
      def unapply(cOpt: Option[Char]) = cOpt.flatMap { c ⇒ if (pred(c)) Some(c) else None }
    }

    val UpArrow = mkMatcher(isUpArrow)
    val DownArrow = mkMatcher(isDownArrow)
    val LeftArrow = mkMatcher(isLeftArrow)
    val RightArrow = mkMatcher(isRightArrow)
    val EdgeCrossing = mkMatcher(isEdgeCrossing)
    val EdgeChar = mkMatcher(isEdge)
    val Horizontal = mkMatcher(isHorizontalEdge)
    val Vertical = mkMatcher(isVerticalEdge)
    val HorizOrVert = mkMatcher(c ⇒ isVerticalEdge(c) || isHorizontalEdge(c))
    val VertOrCrossing = mkMatcher(c ⇒ isVerticalEdge(c) || isEdgeCrossing(c))
    val HorizOrCrossing = mkMatcher(c ⇒ isHorizontalEdge(c) || isEdgeCrossing(c))

    val RightToDown = mkMatcher(isRightToDownEdgeBend)
    val RightToUp = mkMatcher(isRightToUpEdgeBend)
    val LeftToDown = mkMatcher(isLeftToDownEdgeBend)
    val LeftToUp = mkMatcher(isLeftToUpEdgeBend)

    if (direction.isHorizontal)
      (charAtOpt(ahead), charAtOpt(ahead.go(Up)), charAtOpt(ahead.go(Down)), charAtOpt(ahead.go(direction))) match {
        case _ if isBoxEdge(ahead)                                     ⇒ finaliseEdge(ahead)
        case (EdgeCrossing(_), _, _, _)                                ⇒ followEdge(direction, ahead :: edgeSoFar)
        case (DownArrow(_), _, _, _)                                   ⇒ followEdge(Down, ahead :: edgeSoFar)
        case (UpArrow(_), _, _, _)                                     ⇒ followEdge(Up, ahead :: edgeSoFar)
        case (Horizontal(_), _, _, EdgeChar(_))                        ⇒ followEdge(direction, ahead :: edgeSoFar)
        case (RightToUp(_), _, _, _) if direction == Right             ⇒ followEdge(Up, ahead :: edgeSoFar)
        case (RightToDown(_), _, _, _) if direction == Right           ⇒ followEdge(Down, ahead :: edgeSoFar)
        case (LeftToUp(_), _, _, _) if direction == Left               ⇒ followEdge(Up, ahead :: edgeSoFar)
        case (LeftToDown(_), _, _, _) if direction == Left             ⇒ followEdge(Down, ahead :: edgeSoFar)
        case (HorizOrVert(_), UpArrow(_), DownArrow(_), _)             ⇒ None
        case (HorizOrVert(_), UpArrow(_), _, _)                        ⇒ followEdge(Up, ahead :: edgeSoFar)
        case (HorizOrVert(_), _, DownArrow(_), _)                      ⇒ followEdge(Down, ahead :: edgeSoFar)
        case (HorizOrVert(_), VertOrCrossing(_), VertOrCrossing(_), _) ⇒ None
        case (HorizOrVert(_), VertOrCrossing(_), _, _)                 ⇒ followEdge(Up, ahead :: edgeSoFar)
        case (HorizOrVert(_), _, VertOrCrossing(_), _)                 ⇒ followEdge(Down, ahead :: edgeSoFar)
        case (Vertical(_), Horizontal(_), Horizontal(_), _)            ⇒ None
        case (Vertical(_), Horizontal(_), _, _)                        ⇒ followEdge(Up, ahead :: edgeSoFar)
        case (Vertical(_), _, Horizontal(_), _)                        ⇒ followEdge(Down, ahead :: edgeSoFar)
        case (LeftArrow(_), _, _, _) if direction == Left              ⇒ followEdge(direction, ahead :: edgeSoFar)
        case (RightArrow(_), _, _, _) if direction == Right            ⇒ followEdge(direction, ahead :: edgeSoFar)
        case _                                                         ⇒ None
      }
    else
      (charAtOpt(ahead), charAtOpt(ahead.go(Left)), charAtOpt(ahead.go(Right)), charAtOpt(ahead.go(direction))) match {
        case _ if isBoxEdge(ahead)                                       ⇒ finaliseEdge(ahead)
        case (EdgeCrossing(_), _, _, _)                                  ⇒ followEdge(direction, ahead :: edgeSoFar)
        case (LeftArrow(_), _, _, _)                                     ⇒ followEdge(Left, ahead :: edgeSoFar)
        case (RightArrow(_), _, _, _)                                    ⇒ followEdge(Right, ahead :: edgeSoFar)
        case (Vertical(_), _, _, EdgeChar(_))                            ⇒ followEdge(direction, ahead :: edgeSoFar)
        case (RightToUp(_), _, _, _) if direction == Down                ⇒ followEdge(Left, ahead :: edgeSoFar)
        case (RightToDown(_), _, _, _) if direction == Up                ⇒ followEdge(Left, ahead :: edgeSoFar)
        case (LeftToUp(_), _, _, _) if direction == Down                 ⇒ followEdge(Right, ahead :: edgeSoFar)
        case (LeftToDown(_), _, _, _) if direction == Up                 ⇒ followEdge(Right, ahead :: edgeSoFar)
        case (HorizOrVert(_), LeftArrow(_), RightArrow(_), _)            ⇒ None
        case (HorizOrVert(_), LeftArrow(_), _, _)                        ⇒ followEdge(Left, ahead :: edgeSoFar)
        case (HorizOrVert(_), _, RightArrow(_), _)                       ⇒ followEdge(Right, ahead :: edgeSoFar)
        case (HorizOrVert(_), HorizOrCrossing(_), HorizOrCrossing(_), _) ⇒ None
        case (HorizOrVert(_), HorizOrCrossing(_), _, _)                  ⇒ followEdge(Left, ahead :: edgeSoFar)
        case (HorizOrVert(_), _, HorizOrCrossing(_), _)                  ⇒ followEdge(Right, ahead :: edgeSoFar)
        case (Horizontal(_), Vertical(_), Vertical(_), _)                ⇒ None
        case (Horizontal(_), Vertical(_), _, _)                          ⇒ followEdge(Left, ahead :: edgeSoFar)
        case (Horizontal(_), _, Vertical(_), _)                          ⇒ followEdge(Right, ahead :: edgeSoFar)
        case (UpArrow(_), _, _, _) if direction == Up                    ⇒ followEdge(direction, ahead :: edgeSoFar)
        case (DownArrow(_), _, _, _) if direction == Down                ⇒ followEdge(direction, ahead :: edgeSoFar)
        case _                                                           ⇒ None
      }
  }

  private def followEdge(direction: Direction, startPoint: Point): Option[EdgeImpl] =
    if (isEdgeStart(charAt(startPoint), direction))
      followEdge2(startPoint.go(direction) :: startPoint :: Nil, direction)
    else if (!isUnicode(charAt(startPoint)))
      followEdge(direction, startPoint :: Nil)
    else
      None

  @tailrec
  private def followEdge2(points: List[Point], direction: Direction): Option[EdgeImpl] = {
    val currentPoint = points.head
    if (!inDiagram(currentPoint))
      return None
    val c = charAt(currentPoint)
    if (isBoxEdge(currentPoint))
      Some(new EdgeImpl(points.reverse))
    else if (isStraightAhead(c, direction) || isCrossing(c) || isAheadArrow(c, direction))
      followEdge2(currentPoint.go(direction) :: points, direction)
    else if (isLeftTurn(c, direction))
      followEdge2(currentPoint.go(direction.turnLeft) :: points, direction.turnLeft)
    else if (isRightTurn(c, direction))
      followEdge2(currentPoint.go(direction.turnRight) :: points, direction.turnRight)
    else
      None
  }

  private def isEdgeStart(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('╤' | '┬', Down)         ⇒ true
    case ('╪' | '┼', Up | Down)    ⇒ true
    case ('╧' | '┴', Up)           ⇒ true
    case ('╟' | '├', Right)        ⇒ true
    case ('╫' | '┼', Right | Left) ⇒ true
    case ('╢' | '┤', Left)         ⇒ true
  }

  private def isStraightAhead(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('─', Right | Left) ⇒ true
    case ('│', Up | Down)    ⇒ true
  }

  private def isAheadArrow(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('^', Up)         ⇒ true
    case ('v' | 'V', Down) ⇒ true
    case ('<', Left)       ⇒ true
    case ('>', Right)      ⇒ true
  }

  private def isLeftTurn(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('╮' | '┐', Up)    ⇒ true
    case ('╯' | '┘', Right) ⇒ true
    case ('╭' | '┌', Left)  ⇒ true
    case ('╰' | '└', Down)  ⇒ true
  }

  private def isRightTurn(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('╮' | '┐', Right) ⇒ true
    case ('╯' | '┘', Down)  ⇒ true
    case ('╭' | '┌', Up)    ⇒ true
    case ('╰' | '└', Left)  ⇒ true
  }

  private def isCrossing(c: Char): Boolean = c == '┼'

  private val edges =
    allBoxes.flatMap { box ⇒
      box.rightBoundary.flatMap(followEdge(Right, _)) ++
        box.leftBoundary.flatMap(followEdge(Left, _)) ++
        box.topBoundary.flatMap(followEdge(Up, _)) ++
        box.bottomBoundary.flatMap(followEdge(Down, _))
    }

  diagram.allEdges = edges.groupBy(_.points.toSet).values.toList.map(_.head)
  for (edge ← diagram.allEdges) {
    edge.box1.edges ::= edge
    if (edge.box1 != edge.box2)
      edge.box2.edges ::= edge
  }

  val allEdgePoints = diagram.allEdges.flatMap(_.points)

  def collectText(container: ContainerImpl): String = {
    val region = container.contentsRegion
    val allPoints = (container.childBoxes.flatMap(_.region.points) ++ allEdgePoints ++ allLabelPoints).toSet
    val sb = new StringBuilder
    for (row ← region.topLeft.row to region.bottomRight.row) {
      for {
        column ← region.topLeft.column to region.bottomRight.column
        point = Point(row, column)
        if !allPoints.contains(point)
        c = charAt(point)
      } sb.append(c)
      sb.append("\n")
    }
    if (sb.nonEmpty)
      sb.deleteCharAt(sb.length - 1)
    sb.toString
  }

  for (edge ← diagram.allEdges) {
    val labels: Set[Label] =
      (for {
        point ← edge.points
        startPoint ← point.neighbours
        ('[' | ']') ← charAtOpt(startPoint)
        label ← completeLabel(startPoint, edge.parent)
      } yield label).toSet
    if (labels.size > 1)
      throw new DiagramParseException("Multiple labels for edge " + edge + ", " + labels.map(_.text).mkString(","))
    edge.label_ = labels.headOption
  }

  private lazy val allLabelPoints: Set[Point] =
    (for {
      edge ← diagram.allEdges
      label ← edge.label_.toList
      point ← label.points
    } yield point).toSet

  for (box ← allBoxes)
    box.text = collectText(box)
  diagram.text = collectText(diagram)

  private def completeLabel(startPoint: Point, parent: ContainerImpl): Option[Label] = {
    val occupiedPoints = parent.childBoxes.flatMap(_.region.points) ++ allEdgePoints toSet
    val (finalChar, direction) = charAt(startPoint) match {
      case '[' ⇒ (']', Right)
      case ']' ⇒ ('[', Left)
    }

    def search(point: Point): Option[Label] = charAtOpt(point) flatMap {
      case `finalChar` ⇒
        val List(p1, p2) = List(startPoint, point).sortBy(_.column)
        Some(Label(p1, p2))
      case _ if occupiedPoints.contains(point) ⇒
        None
      case _ ⇒
        search(point.go(direction))
    }

    search(startPoint.go(direction))
  }

  private abstract class ContainerImpl extends RegionToString { self: Container ⇒

    var text: String = ""

    var childBoxes: List[BoxImpl] = Nil

    def contentsRegion: Region

  }

  private class BoxImpl(val topLeft: Point, val bottomRight: Point) extends ContainerImpl with Box {

    var edges: List[EdgeImpl] = Nil

    var parent: Option[Container with ContainerImpl] = None

    def region: Region = Region(topLeft, bottomRight)

    def contentsRegion: Region = Region(topLeft.right.down, bottomRight.up.left)

    val leftBoundary: List[Point] = for (row ← topLeft.row to bottomRight.row toList) yield Point(row, topLeft.column)
    val rightBoundary: List[Point] = for (row ← topLeft.row to bottomRight.row toList) yield Point(row, bottomRight.column)
    val topBoundary: List[Point] = for (column ← topLeft.column to bottomRight.column toList) yield Point(topLeft.row, column)
    val bottomBoundary: List[Point] = for (column ← topLeft.column to bottomRight.column toList) yield Point(bottomRight.row, column)

    val boundaryPoints: Set[Point] = leftBoundary.toSet ++ rightBoundary.toSet ++ topBoundary.toSet ++ bottomBoundary.toSet

  }

  private class EdgeImpl(val points: List[Point]) extends Edge {

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
          points.map(_.column).min),
        Point(
          points.map(_.row).max,
          points.map(_.column).max))

  }

  private trait RegionToString {

    def region: Region

    override def toString = diagramRegionToString(region)

  }

  def diagramRegionToString(region: Region, includePoint: Point ⇒ Boolean = p ⇒ true) = {
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

  private class DiagramImpl(numberOfRows: Int, numberOfColumns: Int) extends ContainerImpl with Diagram {

    var allBoxes: List[BoxImpl] = Nil

    var allEdges: List[EdgeImpl] = Nil

    def boxAt(point: Point): Option[BoxImpl] = allBoxes.find(_.boundaryPoints.contains(point))

    def region: Region = diagramRegion

    def contentsRegion = region

  }

  private case class Label(start: Point, end: Point) {

    require(start.row == end.row)

    val row = start.row

    def points: List[Point] =
      for (column ← start.column to end.column toList)
        yield Point(row, column)

    val text: String = {
      val sb = new StringBuilder
      for (column ← start.column + 1 to end.column - 1)
        sb.append(charAt(Point(row, column)))
      sb.toString
    }

  }

}