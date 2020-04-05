package org.opentorah.collector

abstract class SiteFile(val siteObject: SiteObject) {
  def url: Seq[String]
}

object SiteFile {

  def resolve(extension: Option[String], k: => SiteObject): Option[SiteFile] = extension match {
    case Some("html") => Some(k.teiWrapperFile)
    case Some("xml") => Some(k.teiFile)
    case _ => None
  }
}
