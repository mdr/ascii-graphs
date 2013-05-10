package com.github.mdr.ascii.layout.cycles

import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import com.github.mdr.ascii.graph.Graph

/**
 * Tracks information useful during cycle removal algorithm:
 *   - vertices which have been deleted from the graph
 *   - sources and sinks
 *   - vertices which have the largest difference between the out-degree and in-degree
 */
class CycleRemovalInfo[V](graph: Graph[V]) {

  private var sources: Set[V] = Set() // excludes isolated vertices

  private var sinks: Set[V] = Set() // includes isolated vertices

  /**
   * For each vertex v, the difference between the out- and in-degrees: outDegree(v) - inDegree(v)
   */
  private var verticesToDegreeDiff: Map[V, Int] = Map()

  /**
   * Inverse of verticesToDegreeDiff.
   */
  private var degreeDiffToVertices: SortedMap[Int, List[V]] = SortedMap() // out degree - in degree

  private var deletedVertices: Set[V] = Set()

  // Initialise based on entire graph:
  for (v ← graph.vertices) {
    val outDegree = graph.outDegree(v)
    val inDegree = graph.inDegree(v)
    if (outDegree == 0)
      sinks += v
    else if (inDegree == 0)
      sources += v
    addVertexToDegreeDiffMaps(v, outDegree - inDegree)
  }

  def getSources: Set[V] = sources

  def getSinks: Set[V] = sinks

  def getLargestDegreeDiffVertex: Option[V] = degreeDiffToVertices.lastOption.flatMap(_._2.headOption)

  def removeVertex(v: V) {
    deletedVertices += v
    if (sinks contains v)
      sinks -= v
    if (sources contains v)
      sources -= v
    removeVertexFromDegreeDiffMaps(v)

    for (outVertex ← getOutVertices(v)) {
      adjustDegreeDiff(outVertex, +1)
      if (getInVertices(outVertex).isEmpty && !sinks.contains(outVertex))
        sources += outVertex
    }
    for (inVertex ← getInVertices(v)) {
      adjustDegreeDiff(inVertex, -1)
      if (getOutVertices(inVertex).isEmpty)
        sinks += inVertex
    }
  }

  private def getInVertices(v: V): List[V] = graph.inVertices(v).filterNot(deletedVertices)

  private def getOutVertices(v: V): List[V] = graph.outVertices(v).filterNot(deletedVertices)

  private def adjustDegreeDiff(v: V, delta: Int) = {
    val previousDegreeDiff = removeVertexFromDegreeDiffMaps(v)
    addVertexToDegreeDiffMaps(v, previousDegreeDiff + delta)
  }

  private def addVertexToDegreeDiffMaps(v: V, degreeDiff: Int) = {
    degreeDiffToVertices += degreeDiff -> (v :: degreeDiffToVertices.getOrElse(degreeDiff, Nil))
    verticesToDegreeDiff += v -> degreeDiff
  }

  private def removeVertexFromDegreeDiffMaps(v: V): Int = {
    val degreeDiff = verticesToDegreeDiff(v)
    val vertices = degreeDiffToVertices(degreeDiff)
    val updatedVertices = vertices.filterNot(_ == v)
    if (updatedVertices.isEmpty)
      degreeDiffToVertices -= degreeDiff
    else
      degreeDiffToVertices += degreeDiff -> updatedVertices
    verticesToDegreeDiff -= v
    degreeDiff
  }

}