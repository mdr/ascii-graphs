import scala.annotation.tailrec

case class Point(row: Int, column: Int) {

  def up = copy(row = row - 1)

  def down = copy(row = row + 1)

  def left = copy(column = column - 1)

  def right = copy(column = column + 1)

  def go(direction: Direction) = direction match {
    case Up    ⇒ up
    case Down  ⇒ down
    case Left  ⇒ left
    case Right ⇒ right
  }

}

sealed trait Direction {
  val turnLeft: Direction
  val turnRight: Direction
}

case object Up extends Direction {
  val turnLeft = Left
  val turnRight = Right
}

case object Down extends Direction {
  val turnLeft = Right
  val turnRight = Left
}

case object Left extends Direction {
  val turnLeft = Down
  val turnRight = Up
}

case object Right extends Direction {
  val turnLeft = Up
  val turnRight = Down

}

case class Region(topLeft: Point, bottomRight: Point) {

  def contains(point: Point): Boolean = {
    point.row >= topLeft.row && point.column >= topLeft.column &&
      point.row <= bottomRight.row && point.column <= bottomRight.column
  }

  def contains(region: Region): Boolean = contains(region.topLeft) && contains(region.bottomRight)

  def intersects(region: Region): Boolean = {
    val disjoint =
      bottomRight.column < region.topLeft.column ||
        region.bottomRight.column < topLeft.column ||
        bottomRight.row < region.topLeft.row ||
        region.bottomRight.row < topLeft.row
    !disjoint
  }

  def width = bottomRight.column - topLeft.column + 1

  def height = bottomRight.row - topLeft.row + 1

  def area = width * height

  def points: List[Point] =
    for {
      row ← (topLeft.row to bottomRight.row toList)
      column ← topLeft.column to bottomRight.column
    } yield Point(row, column)

}

class DiagramParse(s: String) {

  val rawRows: List[String] = if (s.isEmpty) Nil else s.split("(\r)?\n").toList

  val numberOfColumns = rawRows.maxBy(_.length).length

  val rows = rawRows.map(_.padTo(numberOfColumns, ' ')).toArray

  private val numberOfRows = rows.length

  private def charAt(point: Point): Char = rows(point.row)(point.column)

  private def inDiagram(point: Point) = point match {
    case Point(row, column) ⇒
      row >= 0 && column >= 0 && row < numberOfRows && column < numberOfColumns
  }

  @tailrec
  private def scanBoxEdgeRight(start: Point): Option[Point] = charAt(start) match {
    case '+'                           ⇒ Some(start)
    case '-' if inDiagram(start.right) ⇒ scanBoxEdgeRight(start.right)
    case _                             ⇒ None
  }

  @tailrec
  private def scanBoxEdgeDown(start: Point): Option[Point] = charAt(start) match {
    case '+'                          ⇒ Some(start)
    case '|' if inDiagram(start.down) ⇒ scanBoxEdgeDown(start.down)
    case _                            ⇒ None
  }

  /**
   * @return bottomRight of box if all edges are correct
   */
  private def completeBox(topLeft: Point): Option[Point] =
    for {
      topRight ← scanBoxEdgeRight(topLeft.right)
      bottomRight ← scanBoxEdgeDown(topRight.down)
      bottomLeft ← scanBoxEdgeDown(topLeft.down)
      bottomRight2 ← scanBoxEdgeRight(bottomLeft.right)
      if bottomRight == bottomRight2
    } yield bottomRight

  private val possibleTopLefts: List[Point] =
    for {
      row ← (0 until numberOfRows - 1).toList
      column ← 0 until numberOfColumns - 1
      val point = Point(row, column)
      if charAt(point) == '+'
      if charAt(point.right) == '-'
      if charAt(point.down) == '|'
    } yield point

  private val allBoxes =
    for {
      topLeft ← possibleTopLefts
      bottomRight ← completeBox(topLeft)
    } yield new BoxImpl(topLeft, bottomRight)

  val diagram = new DiagramImpl(numberOfRows, numberOfColumns)
  diagram.allBoxes = allBoxes

  val boxContains: Map[BoxImpl, BoxImpl] =
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
    childBox.parentBox = Some(parentBox)
    parentBox.childBoxes ::= childBox
  }

  for (box ← allBoxes if box.parentBox.isEmpty)
    diagram.topLevelBoxes ::= box

  for (box ← allBoxes)
    box.text = {
      for (point ← box.innerRegion.points if !box.childBoxes.exists(_.region contains point))
        yield charAt(point)
    }.mkString

  diagram.text = {
    for (point ← diagram.region.points if !diagram.topLevelBoxes.exists(_.region contains point))
      yield charAt(point)
  }.mkString

  @tailrec
  private def followEdge(direction: Direction, edgeSoFar: List[Point]): Option[EdgeImpl] = {
    val currentPoint = edgeSoFar.head
    if (!inDiagram(currentPoint))
      return None
    //    if (edgeSoFar.size > 2)
    //    println("(" + edgeSoFar.size + ") Tracing edge " + currentPoint + " heading " + direction)
    def charAtOpt(point: Point): Option[Char] = if (inDiagram(point)) Some(charAt(point)) else None
    def isBoxEdge(point: Point) = inDiagram(point) && allBoxes.exists(_.boundaryPoints.contains(point))
    def finaliseEdge(connectPoint: Point): Option[EdgeImpl] = {
      val points = (connectPoint :: edgeSoFar).reverse
      if (points.size <= 2)
        None
      else {
        val e = new EdgeImpl(points)
        //        println("Found edge " + e.points.mkString(", "))
        Some(e)
      }
    }
    val ahead: Point = currentPoint.go(direction)

    def isEdgeChar(c: Char) = c match {
      case '-' | '+' | 'v' | 'V' | '^' | '>' | '<' | '|' ⇒ true
      case _                                             ⇒ false
    }

    if (direction == Left || direction == Right)
      (charAtOpt(ahead), charAtOpt(ahead.go(Up)), charAtOpt(ahead.go(Down)), charAtOpt(ahead.go(direction))) match {
        case _ if isBoxEdge(ahead)                                  ⇒ finaliseEdge(ahead)
        case (Some('+'), _, _, _)                                   ⇒ followEdge(direction, ahead :: edgeSoFar)
        case (Some('v' | 'V'), _, _, _)                             ⇒ followEdge(Down, ahead :: edgeSoFar)
        case (Some('^'), _, _, _)                                   ⇒ followEdge(Up, ahead :: edgeSoFar)
        case (Some('-'), _, _, Some(c)) if isEdgeChar(c)            ⇒ followEdge(direction, ahead :: edgeSoFar)
        case (Some('-' | '|'), Some('^'), Some('v' | 'V'), _)       ⇒ throw new RuntimeException("Ambiguous turn at " + ahead)
        case (Some('-' | '|'), Some('^'), _, _)                     ⇒ followEdge(Up, ahead :: edgeSoFar)
        case (Some('-' | '|'), _, Some('v' | 'V'), _)               ⇒ followEdge(Down, ahead :: edgeSoFar)
        case (Some('-' | '|'), Some('|' | '+'), Some('|' | '+'), _) ⇒ throw new RuntimeException("Ambiguous turn at " + ahead)
        case (Some('-' | '|'), Some('|' | '+'), _, _)               ⇒ followEdge(Up, ahead :: edgeSoFar)
        case (Some('-' | '|'), _, Some('|' | '+'), _)               ⇒ followEdge(Down, ahead :: edgeSoFar)
        case (Some('|'), Some('-'), Some('-'), _)                   ⇒ throw new RuntimeException("Ambiguous turn at " + ahead)
        case (Some('|'), Some('-'), _, _)                           ⇒ followEdge(Up, ahead :: edgeSoFar)
        case (Some('|'), _, Some('-'), _)                           ⇒ followEdge(Down, ahead :: edgeSoFar)
        case (Some('<'), _, _, _) if direction == Left              ⇒ followEdge(direction, ahead :: edgeSoFar)
        case (Some('>'), _, _, _) if direction == Right             ⇒ followEdge(direction, ahead :: edgeSoFar)
        case _                                                      ⇒ None
      }
    else
      (charAtOpt(ahead), charAtOpt(ahead.go(Left)), charAtOpt(ahead.go(Right)), charAtOpt(ahead.go(direction))) match {
        case _ if isBoxEdge(ahead)                                  ⇒ finaliseEdge(ahead)
        case (Some('+'), _, _, _)                                   ⇒ followEdge(direction, ahead :: edgeSoFar)
        case (Some('<'), _, _, _)                                   ⇒ followEdge(Left, ahead :: edgeSoFar)
        case (Some('>'), _, _, _)                                   ⇒ followEdge(Right, ahead :: edgeSoFar)
        case (Some('|'), _, _, Some(c)) if isEdgeChar(c)            ⇒ followEdge(direction, ahead :: edgeSoFar)
        case (Some('-' | '|'), Some('<'), Some('>'), _)             ⇒ throw new RuntimeException("Ambiguous turn at " + ahead)
        case (Some('-' | '|'), Some('<'), _, _)                     ⇒ followEdge(Left, ahead :: edgeSoFar)
        case (Some('-' | '|'), _, Some('>'), _)                     ⇒ followEdge(Right, ahead :: edgeSoFar)
        case (Some('-' | '|'), Some('-' | '+'), Some('-' | '+'), _) ⇒ throw new RuntimeException("Ambiguous turn at " + ahead)
        case (Some('-' | '|'), Some('-' | '+'), _, _)               ⇒ followEdge(Left, ahead :: edgeSoFar)
        case (Some('-' | '|'), _, Some('-' | '+'), _)               ⇒ followEdge(Right, ahead :: edgeSoFar)
        case (Some('-'), Some('|'), Some('|'), _)                   ⇒ throw new RuntimeException("Ambiguous turn at " + ahead)
        case (Some('-'), Some('|'), _, _)                           ⇒ followEdge(Left, ahead :: edgeSoFar)
        case (Some('-'), _, Some('|'), _)                           ⇒ followEdge(Right, ahead :: edgeSoFar)
        case (Some('^'), _, _, _) if direction == Up                ⇒ followEdge(direction, ahead :: edgeSoFar)
        case (Some('V' | 'v'), _, _, _) if direction == Down        ⇒ followEdge(direction, ahead :: edgeSoFar)
        case _                                                      ⇒ None
      }
  }

  def followEdge(direction: Direction, startPoint: Point): Option[EdgeImpl] = followEdge(direction, startPoint :: Nil)

  val edges =
    allBoxes.flatMap { box ⇒
      box.rightBoundary.flatMap(followEdge(Right, _)) ++
        box.leftBoundary.flatMap(followEdge(Left, _)) ++
        box.topBoundary.flatMap(followEdge(Up, _)) ++
        box.bottomBoundary.flatMap(followEdge(Down, _))
    }

  for (edges ← edges.groupBy(_.points.toSet).values) {
    val edge = edges.head
    edge.box1.edges ::= edge
    if (edge.box1 != edge.box2)
      edge.box2.edges ::= edge
    println(edge)
  }

  def boxAt(point: Point): Option[BoxImpl] = allBoxes.find(_.boundaryPoints.contains(point))

  if (allBoxes.size > -1) {
    println(diagram)
    for (box ← allBoxes) {
      //      println(box)
    }
    println()
  }

  class BoxImpl(val topLeft: Point, val bottomRight: Point) extends Box with RegionToString {

    var text: String = ""

    var childBoxes: List[BoxImpl] = Nil

    var parentBox: Option[BoxImpl] = None

    var edges: List[EdgeImpl] = Nil

    def region: Region = Region(topLeft, bottomRight)

    def innerRegion: Region = Region(topLeft.right.down, bottomRight.up.left)

    val leftBoundary: List[Point] = for (row ← topLeft.row to bottomRight.row toList) yield Point(row, topLeft.column)
    val rightBoundary: List[Point] = for (row ← topLeft.row to bottomRight.row toList) yield Point(row, bottomRight.column)
    val topBoundary: List[Point] = for (column ← topLeft.column to bottomRight.column toList) yield Point(topLeft.row, column)
    val bottomBoundary: List[Point] = for (column ← topLeft.column to bottomRight.column toList) yield Point(bottomRight.row, column)

    val boundaryPoints: Set[Point] = leftBoundary.toSet ++ rightBoundary.toSet ++ topBoundary.toSet ++ bottomBoundary.toSet

  }

  def isArrow(c: Char) = c match {
    case '^' | 'v' | 'V' | '>' | '<' ⇒ true
    case _                           ⇒ false
  }

  class EdgeImpl(val points: List[Point]) extends Edge {

    val box1: BoxImpl = boxAt(points.head).get

    val box2: BoxImpl = boxAt(points.last).get

    var label: Option[String] = None

    val arrow1 = isArrow(charAt(points.head))

    val arrow2 = isArrow(charAt(points.last))

    override def toString = diagramRegionToString(region(points))

    def region(points: List[Point]): Region =
      Region(
        Point(
          points.map(_.row).min,
          points.map(_.column).min),
        Point(
          points.map(_.row).max,
          points.map(_.column).max))

  }

  trait RegionToString {

    def region: Region

    override def toString = diagramRegionToString(region)
  }

  def diagramRegionToString(region: Region) = {
    val sb = new StringBuilder("\n")
    for (row ← region.topLeft.row to region.bottomRight.row) {
      for (column ← region.topLeft.column to region.bottomRight.column)
        sb.append(charAt(Point(row, column)))
      sb.append("\n")
    }
    sb.toString
  }

  class DiagramImpl(numberOfRows: Int, numberOfColumns: Int) extends Diagram with RegionToString {

    var text: String = ""

    var allBoxes: List[BoxImpl] = Nil

    var topLevelBoxes: List[BoxImpl] = Nil

    def region: Region = Region(Point(0, 0), Point(numberOfRows - 1, numberOfColumns - 1))

  }

}

class DiagramParserImpl {

  def parse(s: String): Diagram =
    new DiagramParse(s).diagram

}

trait DiagramParser {

  def parse(s: String): Diagram

}

trait HasText {

  def text: String

}

trait Diagram extends HasText {

  def allBoxes: List[Box]

  def topLevelBoxes: List[Box]

}

trait Box extends HasText {

  def childBoxes: List[Box]

  def parentBox: Option[Box]

  def edges: List[Edge]

}

trait Edge {

  def points: List[Point]

  def box1: Box

  def box2: Box

  def label: Option[String]

}
