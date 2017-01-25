package com.github.mdr.ascii.layout.prefs

object LayoutPrefsImpl {

  /**
   * For easy access to the defaults from Java
   */
  val DEFAULT = LayoutPrefsImpl()

}

case class LayoutPrefsImpl(
  removeKinks: Boolean = true,
  compactify: Boolean = true,
  elevateEdges: Boolean = true,
  vertical: Boolean = true,
  unicode: Boolean = true,
  doubleVertices: Boolean = false,
  rounded: Boolean = false, // Typical Windows fonts don't render bend characters correctly
  explicitAsciiBends: Boolean = false
)
    extends LayoutPrefs
