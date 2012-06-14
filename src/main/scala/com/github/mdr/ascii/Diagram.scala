package com.github.mdr.ascii

class GraphParserException(message: String) extends RuntimeException(message)

trait DiagramParser {

  @throws(classOf[GraphParserException])
  def parse(s: String): Diagram

}

sealed trait Container {

  def text: String

  def region: Region

  /**
   * @return all boxes immediately below this container
   */
  def childBoxes: List[Box]

  def parent: Option[Container]

}

trait Diagram extends Container {

  /**
   * @return all boxes in diagram, whether top level or nested
   */
  def allBoxes: List[Box]

  def allEdges: List[Edge]

  def parent: Option[Container] = None

  def boxAt(point: Point): Option[Box]

}

trait Box extends Container {

  /**
   * @return all edges incident to this box
   */
  def edges: List[Edge]

}

trait Edge {

  def points: List[Point]

  def parent: Container
  
  def box1: Box

  def box2: Box

  val arrow1: Boolean

  val arrow2: Boolean

  def label: Option[String]

}

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
  
  def neighbours: List[Point] = List(up, right, down, left)

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
      this.bottomRight.column < region.topLeft.column ||
        region.bottomRight.column < this.topLeft.column ||
        this.bottomRight.row < region.topLeft.row ||
        region.bottomRight.row < this.topLeft.row
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
