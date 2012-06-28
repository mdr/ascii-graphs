package com.github.mdr.ascii.layout

import com.github.mdr.ascii.Dimension
import com.github.mdr.ascii.Point

case class Drawing(elements: List[DrawingElement]) {

  def dimension: Dimension = {
    val largestPoint =
      elements.flatMap {
        case element: VertexDrawingElement ⇒ List(element.region.bottomRight)
        case element: EdgeDrawingElement   ⇒ element.points
      }.foldLeft(Point(0, 0))(_ maxRowCol _)
    Dimension(width = largestPoint.column + 1, height = largestPoint.row + 1)
  }

}

