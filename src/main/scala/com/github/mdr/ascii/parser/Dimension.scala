package com.github.mdr.ascii.parser

case class Dimension(height: Int, width: Int) {

  def transpose = Dimension(width, height)

}
