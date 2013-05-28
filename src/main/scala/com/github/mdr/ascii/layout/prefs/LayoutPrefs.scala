package com.github.mdr.ascii.layout.prefs

trait LayoutPrefs extends RendererPrefs {

  def removeKinks: Boolean

  def compactify: Boolean

  def vertical: Boolean

}