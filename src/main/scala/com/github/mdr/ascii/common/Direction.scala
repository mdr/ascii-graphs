package com.github.mdr.ascii.common

sealed trait Direction {

  import Direction._

  val turnLeft: Direction

  val turnRight: Direction

  val opposite: Direction

  def isVertical = this == Up || this == Down

  def isHorizontal = !isVertical

}

object Direction {

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

}
