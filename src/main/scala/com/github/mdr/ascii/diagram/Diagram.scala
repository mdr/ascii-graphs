package com.github.mdr.ascii.diagram

import com.github.mdr.ascii.common._
import com.github.mdr.ascii.diagram.parser.DiagramParse

class GraphParserException(message: String) extends RuntimeException(message)

object Diagram {

  @throws(classOf[GraphParserException])
  def apply(s: String): Diagram = new DiagramParse(s).getDiagram

}

trait Diagram extends Container {

  /**
   * @return all boxes in diagram, whether top level or nested
   */
  def allBoxes: List[Box]

  def allEdges: List[Edge]

  def parent: Option[Container] = None

  def boxAt(point: Point): Option[Box]

}

sealed trait Container {

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

trait Box extends Container {

  /**
   * @return all edges incident to this box
   */
  def edges: List[Edge]

  /**
   * @return edges and other boxes incident to this box, filtered according to the given mode.
   */
  def connections(mode: ConnectMode = ConnectMode.All): List[(Edge, Box)] =
    for {
      edge ← edges
      if mode.includeEdge(edge, this)
      otherBox = edge.otherBox(this)
    } yield edge -> otherBox

}
