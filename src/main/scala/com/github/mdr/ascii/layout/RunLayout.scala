package com.github.mdr.ascii.layout

object RunLayout extends Application {

  val v1: Vertex = new RealVertex("1")
  val v2: Vertex = new RealVertex("2")
  val v3: Vertex = new RealVertex("3")
  val v4: Vertex = new RealVertex("4")
  val v5: Vertex = new RealVertex("5")
  val v6: Vertex = new RealVertex("6")
  val v7: Vertex = new RealVertex("7")
  val v8: Vertex = new RealVertex("8")
  val v9: Vertex = new RealVertex("9")
  val va: Vertex = new RealVertex("A")
  val vb: Vertex = new RealVertex("B")
  val vc: Vertex = new RealVertex("C")
  val d1: Vertex = new DummyVertex()
  val d2: Vertex = new DummyVertex()
  val vertexLayers =
    List(
      Layer(v1, v2, v3),
      Layer(v4, v5, v6, v7, v8),
      Layer(v9, d1),
      Layer(va, vb, d2),
      Layer(vc))
  val edges = Nil ++
    List(v3 -> v8, v2 -> v5, v1 -> v6, v2 -> v6, v3 -> v7) ++
    List(v5 -> v9, v7 -> v9, v7 -> d1) ++
    List(v9 -> vb, v9 -> va, d1 -> d2) ++
    List(d2 -> vc)
  //  val layering = Layering(vertexLayers, edges.map { case (x, y) ⇒ new Edge(x, y) })

  val layeringCalculator = new LayeringCalculator[Int]()
  //  val graph = Graph(List("a", "b", "c", "d", "e"), List("a" -> "b", "b" -> "c", "a" -> "c", "d" -> "e"))
  val graph = Graph(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0),
    List(
      1 -> 3,
      1 -> 4,
      3 -> 8,
      2 -> 5,
      2 -> 6,
      3 -> 7,
      5 -> 9,
      7 -> 9))

  val layering = layeringCalculator.assignLayers(graph)
  //  println(layering)
  val result =
    Renderer.render(Layouter.layout(LayerOrderingCalculator.reorder(layering)))

  println(result)

}