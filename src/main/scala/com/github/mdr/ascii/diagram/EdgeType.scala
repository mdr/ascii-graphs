package com.github.mdr.ascii.diagram

sealed trait EdgeType {

  def includeEdge(edge: Edge, thisBox: Box): Boolean

}

object EdgeType {

  /**
   * Return only edges that have an arrow going into the other box
   */
  case object Out extends EdgeType {
    def includeEdge(edge: Edge, fromBox: Box): Boolean = edge.otherHasArrow(fromBox)
  }

  /**
   * Return only edges that have an arrow coming into this box
   */
  case object In extends EdgeType {
    def includeEdge(edge: Edge, fromBox: Box): Boolean = edge.hasArrow(fromBox)
  }

  /**
   * Return only undirected edges
   */
  case object Undirected extends EdgeType {
    def includeEdge(edge: Edge, fromBox: Box): Boolean = edge.hasArrow1 == false && edge.hasArrow2 == false
  }

  /**
   * Return all edges
   */
  case object All extends EdgeType {
    def includeEdge(edge: Edge, fromBox: Box): Boolean = true
  }

}
