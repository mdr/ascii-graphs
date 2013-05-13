package com.github.mdr.ascii.parser

/**
 * Rule for filtering edges incident to a box
 */
trait ConnectMode {
  def includeEdge(edge: Edge, fromBox: Box): Boolean
}

object ConnectMode {

  /**
   * Return only edges that have an arrow going into the other box
   */
  case object Out extends ConnectMode {
    def includeEdge(edge: Edge, fromBox: Box): Boolean = edge.otherHasArrow(fromBox)
  }

  /**
   * Return only edges that have an arrow coming into this box
   */
  case object In extends ConnectMode {
    def includeEdge(edge: Edge, fromBox: Box): Boolean = edge.hasArrow(fromBox)
  }

  /**
   * Return only undirected edges
   */
  case object Undirected extends ConnectMode {
    def includeEdge(edge: Edge, fromBox: Box): Boolean = edge.hasArrow1 == false && edge.hasArrow2 == false
  }

  /**
   * Return all edges
   */
  case object All extends ConnectMode {
    def includeEdge(edge: Edge, fromBox: Box): Boolean = true
  }

}
