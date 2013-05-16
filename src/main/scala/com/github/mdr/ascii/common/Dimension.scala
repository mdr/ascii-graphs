package com.github.mdr.ascii.common

case class Dimension(height: Int, width: Int) {

  def transpose = Dimension(width, height)

}
