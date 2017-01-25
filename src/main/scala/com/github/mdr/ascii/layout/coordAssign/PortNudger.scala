package com.github.mdr.ascii.layout.coordAssign

import com.github.mdr.ascii.layout.layering._
import com.github.mdr.ascii.util.Utils
import com.github.mdr.ascii.common.Point

/**
 * Nudge edge ports to avoid overlapping vertical edge segments.
 *
 * If an edge starts at the same column as another edge finishes, there is a risk they could be drawn overlapping.
 * This avoids the issue by moving the in port one column away (and we make the assumption that there is space
 * for that).
 *
 *    ╭─────╮ ╭─────╮    ╭─────╮ ╭─────╮
 *    │  A  │ │  B  │    │  A  │ │  B  │
 *    ╰─┬─┬─╯ ╰─┬─┬─╯    ╰─┬─┬─╯ ╰─┬─┬─╯
 *      │ │     │ │        │ │     │ │
 *      │ ╰─────┼ │   vs   │ ╰─────┼╮│
 *      │ ╭─────╯ │        │  ╭────╯││
 *      │ │     │ │        │  │     ││
 *      v v     v v        v  v     vv
 *    ╭─────╮ ╭─────╮    ╭─────╮ ╭─────╮
 *    │  X  │ │  Y  │    │  X  │ │  Y  │
 *    ╰─────╯ ╰─────╯    ╰─────╯ ╰─────╯
 *
 * Some of the overlaps can be avoided by ordering the edge rows, but not if the overlaps cause a cycle,
 * as in the case above. Ideally, we would just nudge just enough vertices to break the cycle, and
 * let edge ordering handle the rest, but at the moment we nudge everything.
 */
object PortNudger {

  def nudge(layering: Layering, layerInfos: Map[Layer, LayerInfo]): Map[Layer, LayerInfo] = {
    var updatedLayerInfos = layerInfos
    for ((previousLayerOpt, currentLayer) ← Utils.withPrevious(layering.layers)) yield {
      // Necessary to use use the updatedLayerInfos as we may have nudged edges in the previous layer
      val previousLayerInfoOpt = previousLayerOpt.map(updatedLayerInfos)
      val currentLayerInfo = layerInfos(currentLayer)
      val updatedLayerInfo = nudgeLayer(previousLayerInfoOpt, currentLayerInfo)
      updatedLayerInfos += currentLayer → updatedLayerInfo
    }
    updatedLayerInfos
  }

  private def getOutEdgeColumns(previousLayerInfoOpt: Option[LayerInfo]): List[Int] =
    for {
      previousLayerInfo ← previousLayerInfoOpt.toList
      vertexInfo ← previousLayerInfo.vertexInfos.values
      outPort ← vertexInfo.outEdgeToPortMap.values
    } yield outPort.column

  private def nudgeLayer(previousLayerInfoOpt: Option[LayerInfo], currentLayerInfo: LayerInfo): LayerInfo = {
    val previousEdgeColumns: Set[Int] = getOutEdgeColumns(previousLayerInfoOpt).toSet

    val newVertexInfos = currentLayerInfo.vertexInfos.map {
      case (vertex, vertexInfo) ⇒
        vertex → nudgeVertexInfo(vertex, vertexInfo, previousLayerInfoOpt, previousEdgeColumns)
    }
    currentLayerInfo.copy(vertexInfos = newVertexInfos)
  }

  private def nudgeVertexInfo(vertex: Vertex, vertexInfo: VertexInfo, previousLayerInfoOpt: Option[LayerInfo], previousEdgeColumns: Set[Int]): VertexInfo = {
    def shouldNudge(edge: Edge, port: Point): Boolean =
      previousEdgeColumns.contains(port.column) && !isStraight(edge, vertexInfo, previousLayerInfoOpt)

    var nudgedColumns: Set[Int] = Set()

    val newInEdgeToPortMap = vertexInfo.inEdgeToPortMap.map {
      case (edge, port) if shouldNudge(edge, port) ⇒
        nudgedColumns += port.column
        edge → port.right
      case pair ⇒ pair
    }

    // We need to also nudge the out ports for dummy vertices we nudged earlier
    val newOutEdgeToPortMap = vertex match {
      case _: DummyVertex ⇒ vertexInfo.outEdgeToPortMap.map {
        case (edge, port) if nudgedColumns contains port.column ⇒ edge → port.right
        case pair                                               ⇒ pair
      }
      case _: RealVertex ⇒ vertexInfo.outEdgeToPortMap
    }

    vertexInfo.copy(inEdgeToPortMap = newInEdgeToPortMap, outEdgeToPortMap = newOutEdgeToPortMap)
  }

  private def isStraight(edge: Edge, vertexInfo: VertexInfo, previousLayerInfoOpt: Option[LayerInfo]) = {
    val column1: Option[Int] =
      for {
        previousLayerInfo ← previousLayerInfoOpt
        previousVertexInfo ← previousLayerInfo.vertexInfo(edge.startVertex)
        outPort = previousVertexInfo.outEdgeToPortMap(edge)
      } yield outPort.column

    val column2: Option[Int] = Some(vertexInfo.inEdgeToPortMap(edge).column)
    column1 == column2
  }

}
