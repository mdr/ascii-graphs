package com.github.mdr.ascii.parser

sealed trait Direction {
  val turnLeft: Direction
  val turnRight: Direction
  val opposite: Direction

  def arrow: Char = this match {
    case Up    ⇒ '^'
    case Down  ⇒ 'v'
    case Left  ⇒ '<'
    case Right ⇒ '>'
  }

  def isVertical = this == Up || this == Down

}

case object Up extends Direction {
  val turnLeft = Left
  val turnRight = Right
  val opposite: Direction = Down
}

case object Down extends Direction {
  val turnLeft = Right
  val turnRight = Left
  val opposite: Direction = Up
}

case object Left extends Direction {
  val turnLeft = Down
  val turnRight = Up
  val opposite: Direction = Right
}

case object Right extends Direction {
  val turnLeft = Up
  val turnRight = Down
  val opposite: Direction = Left
}
