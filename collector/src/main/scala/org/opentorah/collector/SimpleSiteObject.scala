package org.opentorah.collector

// When TEI file and its wrapper are in the same directory.
abstract class SimpleSiteObject(site: Site) extends SiteObject(site) {
  final override protected def teiUrl: Seq[String] =
    urlPrefix :+ (fileName + ".xml")

  final override protected def teiWrapperUrl: Seq[String] =
    urlPrefix :+ (fileName + ".html")

  protected def urlPrefix: Seq[String] = Seq.empty

  protected def fileName: String
}

object SimpleSiteObject {

  def resolve(extension: Option[String], k: => SimpleSiteObject): Option[SiteFile] = extension match {
    case Some("html") => Some(k.teiWrapperFile)
    case Some("xml") => Some(k.teiFile)
    case _ => None
  }
}
