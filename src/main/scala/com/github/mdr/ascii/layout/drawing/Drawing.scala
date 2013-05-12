package com.github.mdr.ascii.layout.drawing

import com.github.mdr.ascii.parser.Dimension
import com.github.mdr.ascii.parser.Point

case class Drawing(elements: List[DrawingElement]) {

  lazy val dimension: Dimension = {
    val largestPoint = elements.flatMap(getMaxPoints).foldLeft(Point(-1, -1))(_ maxRowCol _)
    Dimension(width = largestPoint.column + 1, height = largestPoint.row + 1)
  }

  private def getMaxPoints(element: DrawingElement) = element match {
    case element: VertexDrawingElement ⇒ List(element.region.bottomRight)
    case element: EdgeDrawingElement   ⇒ element.bendPoints
  }

  def replaceElement(element: DrawingElement, replacement: DrawingElement) =
    copy(elements = replacement :: elements.filterNot(_ == element))

  def vertexElementAt(point: Point): Option[VertexDrawingElement] =
    elements.collectFirst {
      case vde: VertexDrawingElement if vde.region.contains(point) ⇒ vde
    }

}

