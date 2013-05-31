package com.github.mdr.ascii.layout.drawing

import com.github.mdr.ascii.common._
import com.github.mdr.ascii.layout.prefs.LayoutPrefsImpl

case class Drawing(elements: List[DrawingElement]) extends Transposable[Drawing] {

  lazy val dimension: Dimension = {
    val largestPoint = elements.map(maxPoint).foldLeft(Point(-1, -1))(_ maxRowCol _)
    Dimension.fromPoint(largestPoint)
  }

  private def maxPoint(element: DrawingElement): Point = element match {
    case element: VertexDrawingElement ⇒ element.region.bottomRight
    case element: EdgeDrawingElement   ⇒ element.bendPoints.reduce(_ maxRowCol _)
  }

  def replaceElement(element: DrawingElement, replacement: DrawingElement) =
    copy(elements = replacement :: elements.filterNot(_ == element))

  def vertexElementAt(point: Point): Option[VertexDrawingElement] = elements.collectFirst {
    case vde: VertexDrawingElement if vde.region contains point ⇒ vde
  }

  def vertexElements: List[VertexDrawingElement] = elements.collect {
    case vde: VertexDrawingElement ⇒ vde
  }

  def edgeElements: List[EdgeDrawingElement] = elements.collect {
    case ede: EdgeDrawingElement ⇒ ede
  }

  def transpose: Drawing = Drawing(elements.map(_.transpose))

  override def toString = Renderer.render(this, LayoutPrefsImpl())

}

