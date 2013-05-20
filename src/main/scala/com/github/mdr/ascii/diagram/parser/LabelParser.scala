package com.github.mdr.ascii.diagram.parser

import scala.annotation.tailrec
import com.github.mdr.ascii.common.Point
import com.github.mdr.ascii.common.Direction
import com.github.mdr.ascii.common.Direction._

trait LabelParser { self: DiagramParser ⇒

  protected def getLabel(edge: EdgeImpl): Option[Label] = {
    val labels =
      for {
        point ← edge.points
        startPoint ← point.neighbours
        ('[' | ']') ← charAtOpt(startPoint)
        label ← completeLabel(startPoint, edge.parent)
      } yield label
    if (labels.distinct.size > 1)
      throw new DiagramParserException("Multiple labels for edge " + edge + ", " + labels.distinct.map(_.text).mkString(","))
    else
      labels.headOption
  }

  private def completeLabel(startPoint: Point, parent: ContainerImpl): Option[Label] = {
    val childBoxPoints = parent.childBoxes.flatMap(_.region.points).toSet
    val occupiedPoints = childBoxPoints ++ allEdgePoints
    val (finalChar, direction) = charAt(startPoint) match {
      case '[' ⇒ (']', Right)
      case ']' ⇒ ('[', Left)
    }

    def search(point: Point): Option[Label] = charAtOpt(point) flatMap {
      case `finalChar` ⇒
        val List(p1, p2) = List(startPoint, point).sortBy(_.column)
        Some(Label(p1, p2))
      case _ if occupiedPoints.contains(point) ⇒
        None
      case _ ⇒
        search(point.go(direction))
    }

    search(startPoint.go(direction))
  }

}
