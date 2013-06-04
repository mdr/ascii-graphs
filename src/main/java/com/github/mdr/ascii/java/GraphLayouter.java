package com.github.mdr.ascii.java;

import com.github.mdr.ascii.graph.Graph;
import com.github.mdr.ascii.layout.prefs.LayoutPrefs;
import com.github.mdr.ascii.layout.prefs.LayoutPrefsImpl;

/**
 * Java-friendly utility to configure layout preferences, and layout a grpah.
 */
public class GraphLayouter<V> implements LayoutPrefs {

    private static LayoutPrefsImpl DEFAULT = LayoutPrefsImpl.DEFAULT();

    private boolean removeKinks = DEFAULT.removeKinks();
    private boolean compactify = DEFAULT.compactify();
    private boolean elevateEdges = DEFAULT.elevateEdges();
    private boolean vertical = DEFAULT.vertical();
    private boolean unicode = DEFAULT.unicode();
    private boolean doubleVertices = DEFAULT.doubleVertices();
    private boolean rounded = DEFAULT.rounded();
    private boolean explicitAsciiBends = DEFAULT.explicitAsciiBends();

    public boolean removeKinks() {
        return removeKinks;
    }

    public void setRemoveKinks(boolean removeKinks) {
        this.removeKinks = removeKinks;
    }

    public boolean compactify() {
        return compactify;
    }

    public void setCompactify(boolean compactify) {
        this.compactify = compactify;
    }

    public boolean elevateEdges() {
        return elevateEdges;
    }

    public void setElevateEdges(boolean elevateEdges) {
        this.elevateEdges = elevateEdges;
    }

    public boolean vertical() {
        return vertical;
    }

    public void setVertical(boolean vertical) {
        this.vertical = vertical;
    }

    public boolean unicode() {
        return unicode;
    }

    public void setUnicode(boolean unicode) {
        this.unicode = unicode;
    }

    public boolean doubleVertices() {
        return doubleVertices;
    }

    public void setDoubleVertices(boolean doubleVertices) {
        this.doubleVertices = doubleVertices;
    }

    public boolean rounded() {
        return rounded;
    }

    public void setRounded(boolean rounded) {
        this.rounded = rounded;
    }

    public boolean explicitAsciiBends() {
        return explicitAsciiBends;
    }

    public void setExplicitAsciiBends(boolean explicitAsciiBends) {
        this.explicitAsciiBends = explicitAsciiBends;
    }

    public String layout(Graph<V> graph) {
        return ScalaJavaHelper.renderGraph(graph, this);
    }

}
