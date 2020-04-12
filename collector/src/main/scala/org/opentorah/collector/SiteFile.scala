package org.opentorah.collector

trait SiteFile {

  def siteObject: SiteObject

  def url: Seq[String]

  def content: String
}
