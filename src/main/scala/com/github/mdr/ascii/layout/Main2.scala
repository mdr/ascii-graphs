package com.github.mdr.ascii.layout

import com.github.mdr.ascii.Region
import com.github.mdr.ascii.Point

class Main2 {

  def main(args: Array[String]) {
    val r = new Renderer

    val el = new VertexDrawingElement(Region(Point(10, 2), Point(20, 20)), List("foo"))

    val result = r.render(List(el))
    println(result)

  }
}