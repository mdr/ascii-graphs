package com.github.mdr.ascii.layout.cycles

import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import com.github.mdr.ascii.layout.Graph

class VertexInfoDatabase[V](graph: Graph[V]) {

  def getSources: Set[V] = sources

  def getSinks: Set[V] = sinks

  def getLargestDegreeDiffVertex: Option[V] = degreeDiffToVertices.lastOption.flatMap(_._2.headOption)

  private var sources: Set[V] = Set() // excludes isolated vertices

  private var sinks: Set[V] = Set() // includes isolated vertices

  private var degreeDiffToVertices: SortedMap[Int, List[V]] = SortedMap() // out degree - in degree

  private var verticesToDegreeDiff: Map[V, Int] = Map()

  private var deletedVertices: Set[V] = Set()

  for (v ← graph.vertices) {
    val outDegree = graph.outDegree(v)
    val inDegree = graph.inDegree(v)
    if (outDegree == 0)
      sinks += v
    else if (inDegree == 0)
      sources += v
    val degreeDiff = outDegree - inDegree
    addVertexToDegreeDiffMaps(v, degreeDiff)
  }

  private def getInVertices(v: V): List[V] = graph.inVertices(v).filterNot(deletedVertices)

  private def getOutVertices(v: V): List[V] = graph.outVertices(v).filterNot(deletedVertices)

  def removeVertex(v: V) {
    if (sinks contains v)
      sinks -= v
    if (sources contains v)
      sources -= v
    removeVertexFromDegreeDiffMaps(v)

    val outVertices = getOutVertices(v)
    val inVertices = getInVertices(v)

    deletedVertices += v

    for (outVertex ← outVertices if outVertex != v) {
      val previousDegreeDiff = removeVertexFromDegreeDiffMaps(outVertex)
      addVertexToDegreeDiffMaps(outVertex, previousDegreeDiff + 1)
      if (getInVertices(outVertex).isEmpty)
        sources += outVertex
    }

    for (inVertex ← inVertices if inVertex != v) {
      val previousDegreeDiff = removeVertexFromDegreeDiffMaps(inVertex)
      addVertexToDegreeDiffMaps(inVertex, previousDegreeDiff - 1)
      if (getOutVertices(inVertex).isEmpty)
        sinks += inVertex
    }

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