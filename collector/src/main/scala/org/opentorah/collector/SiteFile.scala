package org.opentorah.collector

trait SiteFile {

  def url: Seq[String]

  def content: String
}
