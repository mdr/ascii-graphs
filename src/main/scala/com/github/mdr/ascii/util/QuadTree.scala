package com.github.mdr.ascii.util

import com.github.mdr.ascii.common._

class QuadTree[T <: HasRegion](dimension: Dimension) {

  private val maxCapacity = 1

  private val allRegion = Region(Point(0, 0), Point(dimension.height - 1, dimension.width - 1))

  private var rootNode: Node = LeafNode(allRegion, items = Set())

  private sealed abstract class Node {

    def region: Region

    def items: Set[T]

    def contains(t: T) = region contains t.region

    def immediateItemsIntersecting(region: Region) =
      items.filter(i ⇒ i.region.intersects(region))

    def immediateItemIntersects(region: Region): Boolean =
      items.exists(i ⇒ i.region.intersects(region))

    def childNodes: List[Node]

    def addItem(t: T): Node

    def removeItem(t: T): Node

  }

  private case class QuadNode(
    region: Region, items: Set[T], topLeft: Node, topRight: Node, bottomLeft: Node, bottomRight: Node
  )
      extends Node {

    override def childNodes: List[Node] = List(topLeft, topRight, bottomLeft, bottomRight)

    def addItem(t: T) = copy(items = items + t)

    def removeItem(t: T) = copy(items = items - t)

  }

  private object QuadNode {

    private val topLeftL = Lens.lens[QuadNode, Node](_.topLeft, (n1, n2) ⇒ n1.copy(topLeft = n2))
    private val topRightL = Lens.lens[QuadNode, Node](_.topRight, (n1, n2) ⇒ n1.copy(topRight = n2))
    private val bottomLeftL = Lens.lens[QuadNode, Node](_.bottomLeft, (n1, n2) ⇒ n1.copy(bottomLeft = n2))
    private val bottomRightL = Lens.lens[QuadNode, Node](_.bottomRight, (n1, n2) ⇒ n1.copy(bottomRight = n2))

    object Lenses {

      val topLeft = topLeftL
      val topRight = topRightL
      val bottomLeft = bottomLeftL
      val bottomRight = bottomRightL

      val childNodeLenses = List(topLeft, topRight, bottomLeft, bottomRight)

    }
  }

  private case class LeafNode(region: Region, items: Set[T]) extends Node {

    override def childNodes: List[Node] = Nil

    def addItem(t: T) = copy(items = items + t)

    def removeItem(t: T) = copy(items = items - t)

  }

  def add(t: T) {
    val region = t.region
    def addRec(n: Node): Node = {
      require(n.region contains region)
      n match {
        case qn: QuadNode ⇒
          QuadNode.Lenses.childNodeLenses.find { lens ⇒ lens(qn).region contains region } match {
            case Some(childLens) ⇒ childLens.update(qn, addRec)
            case None            ⇒ qn.addItem(t)
          }
        case leaf: LeafNode ⇒
          val newLeaf = leaf.addItem(t)

          if (newLeaf.items.size <= maxCapacity && newLeaf.region.width > 1 && newLeaf.region.height > 1)
            newLeaf
          else
            quadrate(newLeaf)
      }
    }
    rootNode = addRec(rootNode)
  }

  def remove(t: T) {
    val region = t.region
    def removeRec(n: Node): Node = {
      require(n.region contains region)
      n match {
        case qn: QuadNode ⇒
          QuadNode.Lenses.childNodeLenses.find(lens ⇒ lens(qn).region contains region) match {
            case Some(childLens) ⇒ childLens.update(qn, removeRec)
            case None            ⇒ n.removeItem(t)
          }
        case leaf: LeafNode ⇒
          n.removeItem(t)
      }
    }
    rootNode = removeRec(rootNode)
  }

  private def quadrate(leaf: LeafNode): QuadNode = {
    val (topLeft, topRight, bottomLeft, bottomRight) = quadrate(leaf.region)

    def makeLeaf(quadrant: Region) = LeafNode(quadrant, leaf.items.filter(i ⇒ quadrant.contains(i.region)))
    val topLeftNode = makeLeaf(topLeft)
    val topRightNode = makeLeaf(topRight)
    val bottomLeftNode = makeLeaf(bottomLeft)
    val bottomRightNode = makeLeaf(bottomRight)

    val newItems = leaf.items.filterNot { i ⇒
      topLeftNode.contains(i) || topRightNode.contains(i) ||
        bottomLeftNode.contains(i) || bottomRightNode.contains(i)
    }
    QuadNode(leaf.region, newItems, topLeftNode, topRightNode, bottomLeftNode, bottomRightNode)
  }

  private def quadrate(region: Region): (Region, Region, Region, Region) = {
    val middleTop = region.topLeft.right(region.width / 2)
    val middleLeft = region.topLeft.down(region.height / 2)
    val middleRight = region.topRight.down(region.height / 2)
    val middleBottom = region.bottomLeft.right(region.width / 2)
    val middle = middleTop.down(region.height / 2)

    val topLeft = Region(region.topLeft, middle.up.left)
    val bottomRight = Region(middle, region.bottomRight)
    val bottomLeft = Region(middleLeft, middleBottom.left)
    val topRight = Region(middleTop, middleRight.up)
    (topLeft, topRight, bottomLeft, bottomRight)
  }

  def collides(region: Region): Boolean = collides(region, rootNode)

  private def collides(region: Region, node: Node): Boolean =
    region.intersects(node.region) && (node.immediateItemIntersects(region) || node.childNodes.exists(collides(region, _)))

  def collisions(t: T): Set[T] = collectCollisions(t.region, rootNode)

  private def collectCollisions(region: Region, node: Node): Set[T] =
    if (region intersects node.region)
      node.immediateItemsIntersecting(region) ++ node.childNodes.flatMap(collectCollisions(region, _))
    else
      Set()

  override def toString = {
    val grid: Array[Array[Char]] = Array.fill(dimension.height, dimension.width)(' ')
    var c = 'a'
    def rec(n: Node): Unit = n match {
      case ln: LeafNode ⇒
        for (point ← ln.region.points)
          grid(point.row)(point.column) = c
        c = (c.toInt + 1).toChar
      case qn: QuadNode ⇒
        qn.childNodes.foreach(rec)
    }
    rec(rootNode)
    grid.map(_.mkString).mkString("\n")
  }

}

object Lens {

  def lens[T, X](getter: T ⇒ X, setter: (T, X) ⇒ T): Lens[T, X] = new Lens[T, X] {
    def get(t: T) = getter(t)
    def set(t: T, x: X) = setter(t, x)
  }

}

trait Lens[T, X] {
  def apply(t: T): X = get(t)
  def get(t: T): X
  def set(t: T, x: X): T
  def update(t: T, f: X ⇒ X): T = set(t, f(get(t)))
}
