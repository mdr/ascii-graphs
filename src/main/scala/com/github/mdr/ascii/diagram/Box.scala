package com.github.mdr.ascii.diagram

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
