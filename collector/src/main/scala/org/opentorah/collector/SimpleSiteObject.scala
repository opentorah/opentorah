package org.opentorah.collector

import org.opentorah.tei.Tei
import scala.xml.Node

abstract class SimpleSiteObject(site: Site) extends SiteObject(site) {

  final override def htmlUrl: Seq[String] =
    urlPrefix :+ (fileName + ".html")

  protected def urlPrefix: Seq[String] = Seq.empty

  protected def fileName: String

  final override protected def tei: Tei = Tei(teiBody)

  protected def teiBody: Seq[Node]
}

object SimpleSiteObject {

  def resolve(extension: Option[String], k: => SimpleSiteObject): Option[SiteFile] = extension match {
    case Some("html") | None => Some(k.htmlFile)
    case _ => None
  }
}
