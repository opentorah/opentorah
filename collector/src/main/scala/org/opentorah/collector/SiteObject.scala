package org.opentorah.collector

import org.opentorah.tei.Tei
import scala.xml.Elem

abstract class SiteObject(val site: Site) {

  def viewer: String

  def teiFile: SiteFile = new SiteFile {
    override def siteObject: SiteObject = SiteObject.this

    override def url: Seq[String] = teiUrl

    override def content: String = {
      val elem: Elem = Site.processTei(Tei.toXml(teiTransformer(tei)), siteObject.site)
      TeiUtil.xmlHeader +
      TeiUtil.teiPrettyPrinter.render(elem) +  "\n"
    }
  }

  def teiWrapperFile: SiteFile = new SiteFile {
    override def siteObject: SiteObject = SiteObject.this

    override def url: Seq[String] = teiWrapperUrl

    final def content: String = Site.withYaml(
      yaml = style.fold[Seq[(String, String)]](Seq.empty)(style => Seq("style" -> style)) ++
        Seq(
          "layout" -> "tei",
          "tei" -> Site.mkUrl(siteObject.teiFile.url),
          "target" -> siteObject.viewer
        ) ++ yaml
    )
  }

  protected def teiUrl: Seq[String]

  protected def teiTransformer: Tei => Tei = TeiUtil.addCommonNoCalendar

  protected def tei: Tei

  protected def teiWrapperUrl: Seq[String]

  protected def style: Option[String] = None

  protected def yaml: Seq[(String, String)] = Seq.empty
}
