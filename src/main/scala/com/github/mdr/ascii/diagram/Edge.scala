package com.github.mdr.ascii.diagram

import com.github.mdr.ascii.common.Point

trait Edge {

  def points: List[Point]

  def parent: Container

  /**
   * First box incident to this edge
   */
  val box1: Box

  /**
   * Second box incident to this edge
   */
  val box2: Box

  /**
   * First box incident to this edge
   */
  val hasArrow1: Boolean

  val hasArrow2: Boolean

  val label: Option[String]

  /**
   * Given one box in the edge, return the other
   */
  def otherBox(box: Box): Box =
    if (box == box1)
      box2
    else if (box == box2)
      box1
    else
      throw new IllegalArgumentException("Box not part of edge: " + box)

  /**
   * Is there an arrow associated with the given box (which must be part of this edge)
   */
  def hasArrow(box: Box): Boolean =
    if (box == box1)
      hasArrow1
    else if (box == box2)
      hasArrow2
    else
      throw new IllegalArgumentException("Box not part of edge: " + box)

  /**
   * Is there an arrow associated with the other box than the given box (which must be part of this edge)
   */
  def otherHasArrow(box: Box): Boolean = hasArrow(otherBox(box))

}