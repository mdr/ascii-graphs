package com.github.mdr.ascii.layout.layering

import com.github.mdr.ascii.layout.layering.LongestDistancesToSinkCalculator._
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

import com.github.mdr.ascii.layout.cycles.CycleRemovalResult
import com.github.mdr.ascii.util.Utils

/**
 * Assign vertices to layers so that, if there is an edge between two vertices, the source vertex is in an
 * earlier layer than the target.
 *
 * In addition, dummy vertices are generated for edges that span multiple layers, corresponding to bends in the
 * displayed edge.
 *
 * Layers are assigned using longest-path layering, with each vertex assigned to a layer corresponding to the
 * longest path from that vertex to a sink.
 */
class LayeringCalculator[V] {

  def assignLayers(cycleRemovalResult: CycleRemovalResult[V]): (Layering, Map[V, RealVertex]) = {
    val graph = cycleRemovalResult.dag

    val distancesToSink = longestDistancesToSink(cycleRemovalResult.dag)
    val maxLayerNum = if (distancesToSink.isEmpty) -1 else distancesToSink.values.max
    def layerNum(v: V): Int = maxLayerNum - distancesToSink(v)

    val layeringBuilder = new LayeringBuilder(maxLayerNum + 1)

    val realVertices: Map[V, RealVertex] = makeRealVertices(cycleRemovalResult)
    for (v ← graph.vertices)
      layeringBuilder.addVertex(layerNum(v), realVertices(v))

    val edges = addEdges(cycleRemovalResult, layerNum, layeringBuilder, realVertices)
    (layeringBuilder.build, realVertices)
  }

  private class LayeringBuilder(numberOfLayers: Int) {

    val layers: Buffer[Buffer[Vertex]] = ListBuffer.fill(numberOfLayers)(ListBuffer[Vertex]())

    var edges: List[Edge] = Nil

    def addVertex(layerNum: Int, v: Vertex) { layers(layerNum) += v }

    def addEdge(edge: Edge) { edges ::= edge }

    def build = Layering(layers.toList.map(layer ⇒ Layer(layer.toList)), edges)
  }

  private def addEdges(cycleRemovalResult: CycleRemovalResult[V], layerNum: V ⇒ Int, layeringBuilder: LayeringBuilder,
    realVertices: Map[V, RealVertex]) {

    // We decrement counts in revEdges as we construct layer edges to make sure the right number of reversed edges
    // are generated:
    var revEdges = Utils.mkMultiset(cycleRemovalResult.reversedEdges)

    for (graphEdge @ (from, to) ← cycleRemovalResult.dag.edges) {
      val fromLayer = layerNum(from)
      val toLayer = layerNum(to)
      val dummies = (fromLayer + 1) to (toLayer - 1) map { layerNum ⇒
        val dummy = new DummyVertex
        layeringBuilder.addVertex(layerNum, dummy)
        dummy
      }
      val vertexChain = realVertices(from) +: dummies :+ realVertices(to)
      val reversed = revEdges.get(graphEdge) match {
        case Some(count) ⇒
          if (count == 1)
            revEdges -= graphEdge
          else
            revEdges += graphEdge → (count - 1)
          true
        case None ⇒
          false
      }
      for ((v1, v2) ← vertexChain.zip(vertexChain.tail))
        layeringBuilder.addEdge(new Edge(v1, v2, reversed))
    }
  }

  private def makeRealVertices(cycleRemovalResult: CycleRemovalResult[V]): Map[V, RealVertex] =
    cycleRemovalResult.dag.vertices.map { v ⇒
      val selfEdges = cycleRemovalResult.countSelfEdges(v)
      v → new RealVertex(v, selfEdges)
    }.toMap

}