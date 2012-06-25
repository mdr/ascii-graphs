package com.github.mdr.ascii.layout

import com.github.mdr.ascii.Dimension

object RunLayout extends Application {

  val graph1 = Graph(
    vertices = List(
      1, 2, 3, 4, 5, 6, 7, 8123, 9, 120),
    edges = List(
      1 -> 3,
      1 -> 4,
      3 -> 8123,
      2 -> 5,
      2 -> 6,
      3 -> 7,
      5 -> 9,
      7 -> 9))

  val V1 = "wibble"
  val V2 = "wobble"
  val V3 = "splish\nsplosh"
  val V4 = "flurble"
  val V5 = "baz\nbiz\nbuz"

  val graph2 = Graph(
    vertices = List(
      V1, V2, V3, V4, V5),
    edges = List(
      V1 -> V2,
      V1 -> V5,
      V5 -> V3,
      V3 -> V4))

  val layeringCalculator = new LayeringCalculator[String]()
  val layering = layeringCalculator.assignLayers(graph2)
  //  println(layering)

  val layouter = new Layouter[Int](ToStringVertexRenderingStrategy)
  val elements = layouter.layout(LayerOrderingCalculator.reorder(layering))
  //  elements.foreach(println)
  val result = Renderer.render(elements)

  println(result)

}