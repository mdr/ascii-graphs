package com.github.mdr.ascii.layout.prefs

case class LayoutPrefsImpl(
  removeKinks: Boolean = true,
  compactify: Boolean = true,
  vertical: Boolean = true,
  unicode: Boolean = false,
  doubleVertices: Boolean = false,
  rounded: Boolean = false,
  explicitAsciiBends: Boolean = true) extends LayoutPrefs
