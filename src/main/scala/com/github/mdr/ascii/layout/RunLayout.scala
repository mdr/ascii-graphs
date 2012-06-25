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

  val V1 = "wicket\norg.apache.wicket\n1.5-M3"
  val V2 = "slf4j-api\norg.slft4j\n1.5.8"
  val V3 = "wicket-request\norg.apache.wicket\n1.5-M3"
  val V4 = "wicket-util\norg.apache.wicket\n1.5-M3"
  val graph2 = Graph(
    vertices = List(
      V1, V2, V3, V4),
    edges = List(
      V1 -> V2,
      V1 -> V3,
      V1 -> V4))

  val layeringCalculator = new LayeringCalculator[String]()
  val layering = layeringCalculator.assignLayers(graph2)
  //  println(layering)

  val layouter = new Layouter[Int](ToStringVertexRenderingStrategy)
  val elements = layouter.layout(LayerOrderingCalculator.reorder(layering))
  //  elements.foreach(println)
  val result = Renderer.render(elements)

  println(result)

}