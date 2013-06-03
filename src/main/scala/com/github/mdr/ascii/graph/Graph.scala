package com.github.mdr.ascii.graph

import scala.PartialFunction.cond
import com.github.mdr.ascii.diagram.Diagram
import com.github.mdr.ascii.layout.GraphLayout
import com.github.mdr.ascii.util.Utils
import com.github.mdr.ascii.util.Utils._
import com.github.mdr.ascii.layout.prefs.LayoutPrefsImpl

object Graph {

  def fromDiagram(s: String): Graph[String] = fromDiagram(Diagram(s))

  def fromDiagram(diagram: Diagram): Graph[String] = DiagramToGraphConvertor.toGraph(diagram)

}

/**
 * A directed graph, allowing multi-edges and loops.
 *
 * Two vertex objects are considered indistinguishable if they compare the same via .equals().
 *
 * @param edges, vertices in the edges must be present in the vertices.
 */
case class Graph[V](vertices: Set[V], edges: List[(V, V)]) {

  val outMap: Map[V, List[V]] = edges.groupBy(_._1).map { case (k, vs) ⇒ (k, vs.map(_._2)) }

  val inMap: Map[V, List[V]] = edges.groupBy(_._2).map { case (k, vs) ⇒ (k, vs.map(_._1)) }

  require(outMap.keys.forall(vertices.contains))

  require(inMap.keys.forall(vertices.contains))

  def isEmpty = vertices.isEmpty

  def inEdges(v: V): List[(V, V)] = edges.filter(_._2 == v)

  def outEdges(v: V): List[(V, V)] = edges.filter(_._1 == v)

  def inVertices(v: V): List[V] = inMap.getOrElse(v, Nil)

  def outVertices(v: V): List[V] = outMap.getOrElse(v, Nil)

  def outDegree(v: V): Int = outVertices(v).size

  def inDegree(v: V): Int = inVertices(v).size

  def sources: List[V] = vertices.toList.filter(inDegree(_) == 0)

  def sinks: List[V] = vertices.toList.filter(outDegree(_) == 0)

  def removeEdge(edge: (V, V)): Graph[V] = copy(edges = Utils.removeFirst(edges, edge))

  def removeVertex(v: V): Graph[V] =
    Graph(vertices.filterNot(_ == v), edges.filterNot { case (v1, v2) ⇒ v1 == v || v2 == v })

  def map[U](f: V ⇒ U): Graph[U] =
    Graph(vertices.map(f), edges.map { case (v1, v2) ⇒ (f(v1), f(v2)) })

  override lazy val hashCode = vertices.## + edges.##

  override def equals(obj: Any): Boolean = cond(obj) {
    case other: Graph[V] ⇒
      multisetCompare(vertices.toList, other.vertices.toList) &&
        multisetCompare(edges, other.edges)
  }

  private def singletonVertices = vertices.filter(v ⇒ inDegree(v) == 0 && outDegree(v) == 0)

  private def asVertexList: String = {
    def render(v: V) = v.toString.replaceAll("\n", "\\\\n")
    singletonVertices.toList.map(render).sorted.mkString("\n") + "\n" +
      edges.toList.sortBy(e ⇒ (render(e._1), render(e._2))).map(e ⇒ render(e._1) + "," + render(e._2)).mkString("\n")
  }

  override def toString =
    try {
      val layoutPrefs = LayoutPrefsImpl(unicode = true, explicitAsciiBends = false)
      "\n" + GraphLayout.renderGraph(this, layoutPrefs = layoutPrefs) + "\n" + asVertexList
    } catch {
      case t: Throwable ⇒ asVertexList
    }

}