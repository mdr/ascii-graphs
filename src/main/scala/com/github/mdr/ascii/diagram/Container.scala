package com.github.mdr.ascii.diagram

import com.github.mdr.ascii.common._

trait Container {

  /**
   * @return all the text directly inside this container, excluding any diagram elements (boxes, edges and labels).
   */
  def text: String

  /**
   * @return the area of the diagram covered by this container
   */
  def region: Region

  /**
   * @return all boxes immediately below this container
   */
  def childBoxes: List[Box]

  def parent: Option[Container]

}