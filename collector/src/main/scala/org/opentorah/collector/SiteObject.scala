package org.opentorah.collector

abstract class SiteObject(val site: Site) {
  def viewer: String

  def teiFile: TeiFile

  def teiWrapperFile: TeiWrapperFile
}
