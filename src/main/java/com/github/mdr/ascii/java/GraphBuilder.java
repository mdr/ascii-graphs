package com.github.mdr.ascii.java;

import static com.github.mdr.ascii.java.ScalaJavaHelper.*;
import com.github.mdr.ascii.graph.Graph;
import java.util.*;
import scala.Tuple2;

/**
 * Utility to build a Graph easily from Java.
 */
public class GraphBuilder<V> {

	private final Set<V> vertices = new HashSet<V>();

	private final List<Tuple2<V, V>> edges = new ArrayList<Tuple2<V, V>>();

	public GraphBuilder<V> addVertex(V v) {
		vertices.add(v);
		return this;
	}

	public GraphBuilder<V> addEdge(V v1, V v2) {
		addVertex(v1).addVertex(v2);
		edges.add(tuple(v1, v2));
		return this;
	}

	public Graph<V> build() {
		return new Graph<V>(asScalaSet(vertices), asScalaList(edges));
	}

}
