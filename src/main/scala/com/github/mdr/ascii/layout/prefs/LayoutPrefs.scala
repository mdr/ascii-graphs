package com.github.mdr.ascii.layout.prefs

trait LayoutPrefs extends RendererPrefs {

  /**
   * Straighten edges by removing kinks wherever possible.
   */
  def removeKinks: Boolean

  /**
   * Shrink diagram by removing edge rows (or columns) that are not needed.
   */
  def compactify: Boolean

  /**
   * Move edges up where possible, to avoid collisions.
   */
  def elevateEdges: Boolean

  /**
   * If true, layers flow from top to bottom. If false, layers flow from left to right.
   */
  def vertical: Boolean

}