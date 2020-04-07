package org.opentorah.collector

import org.opentorah.tei.Tei
import scala.xml.{Elem, Node}

abstract class SiteObject(val site: Site) {

  def viewer: String

  // TODO rename xmlFile
  def teiFile: SiteFile = new SiteFile {
    override def siteObject: SiteObject = SiteObject.this

    override def url: Seq[String] = teiUrl

    override def content: String = {
      val elem: Elem = Site.processTei(Tei.toXml(TeiUtil.addCommonNoCalendar(Tei(xml))), siteObject.site)

      """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" +
        TeiUtil.teiPrettyPrinter.render(elem) +
        "\n"
    }
  }

  // TODO rename htmlFile
  def teiWrapperFile: SiteFile = new SiteFile {
    override def siteObject: SiteObject = SiteObject.this

    override def url: Seq[String] = teiWrapperUrl

    final def content: String = Site.withYaml("tei",
      yaml = style.fold[Seq[(String, String)]](Seq.empty)(style => Seq("style" -> style)) ++
        Seq(
          "tei" -> Site.quote(Site.mkUrl(siteObject.teiFile.url)),
          "target" -> siteObject.viewer
        ) ++ yaml
    )
  }

  protected def teiUrl: Seq[String]

  protected def xml: Seq[Node]

  protected def teiWrapperUrl: Seq[String]

  protected def style: Option[String] = None

  protected def yaml: Seq[(String, String)] = Seq.empty
}
