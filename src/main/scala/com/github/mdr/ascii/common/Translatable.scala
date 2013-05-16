package com.github.mdr.ascii.common

import com.github.mdr.ascii.common.Direction._

/**
 * An object that can be shifted in 2 dimensions.
 */
trait Translatable[+Self] {

  def translate(down: Int = 0, right: Int = 0): Self

  def up: Self = up(1)
  def up(n: Int): Self = translate(down = -n)

  def down: Self = down(1)
  def down(n: Int): Self = translate(down = n)

  def left: Self = left(1)
  def left(n: Int): Self = translate(right = -n)

  def right: Self = right(1)
  def right(n: Int): Self = translate(right = n)

  def go(direction: Direction) = direction match {
    case Up    ⇒ up
    case Down  ⇒ down
    case Left  ⇒ left
    case Right ⇒ right
  }

}
