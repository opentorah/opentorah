package org.opentorah.collector

abstract class SiteObjectWithFile(site: Site) extends SiteObject(site) {
  def htmlFile: SiteFile
}
