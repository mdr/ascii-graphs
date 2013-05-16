package com.github.mdr.ascii.common

trait Transposable[+T] {

  /**
   * Flip this item across the top-left to bottom-right diagonal.
   */
  def transpose: T

}