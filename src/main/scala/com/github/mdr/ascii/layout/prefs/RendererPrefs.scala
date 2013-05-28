package com.github.mdr.ascii.layout.prefs

trait RendererPrefs {

  /**
   * If true, use Unicode box-drawing characters, else use ASCII characters.
   */
  def unicode: Boolean

  /**
   * If true (and unicode is true), use double-lines for rendering boxes.
   */
  def doubleVertices: Boolean

  /**
   * If true (and unicode is true), use rounded corners for edge bends and (if doubleVertices is false) for box corners.
   *   Note: these characters seem not to be well supported by typical Windows fonts.
   */
  def rounded: Boolean

  /**
   * If true (and unicode is false), use / and \ characters for edge bends. This is useful for reducing ambiguity,
   *   but can be more aesthetically noisy.
   */
  def explicitAsciiBends: Boolean

}
