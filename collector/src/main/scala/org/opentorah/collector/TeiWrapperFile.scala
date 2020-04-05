package org.opentorah.collector

// TODO add ".html" to the last segment
abstract class TeiWrapperFile(siteObject: SiteObject) extends SiteFile(siteObject) {
  def style: Option[String] = None

  def yaml: Seq[(String, String)] = Seq.empty
}
