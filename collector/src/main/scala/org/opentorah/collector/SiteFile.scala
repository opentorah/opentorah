package org.opentorah.collector

import scala.xml.Elem

trait SiteFile {

  def url: Seq[String]

  def content: String

  def contentNg(siteParameters: SiteParameters): Elem = ???
}
