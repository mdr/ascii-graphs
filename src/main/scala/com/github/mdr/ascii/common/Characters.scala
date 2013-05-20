package com.github.mdr.ascii.common

import com.github.mdr.ascii.common.Direction._
import scala.PartialFunction.cond

object Characters {

  def isAheadArrow(c: Char, direction: Direction): Boolean = cond(c, direction) {
    case ('^', Up)         ⇒ true
    case ('v' | 'V', Down) ⇒ true
    case ('<', Left)       ⇒ true
    case ('>', Right)      ⇒ true
  }

  def isLeftArrow(c: Char, direction: Direction) = isAheadArrow(c, direction.turnLeft)

  def isRightArrow(c: Char, direction: Direction) = isAheadArrow(c, direction.turnRight)

}