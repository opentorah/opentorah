package org.opentorah.collector

import org.opentorah.tei.Tei
import org.opentorah.xml.LinkResolver
import scala.xml.Node

abstract class TeiSiteObject(site: Site) extends SiteObjectWithFile(site) {

  override val htmlFile: TeiSiteFile = new TeiSiteFile {
    final override def url: Seq[String] = TeiSiteObject.this.url
    final override def viewer: Viewer = TeiSiteObject.this.viewer
    final override def navigationLinks: Seq[NavigationLink] = TeiSiteObject.this.navigationLinks
    final override protected def style: String = TeiSiteObject.this.style
    final override protected def title: Option[String] = TeiSiteObject.this.title
    final override protected def tei: Tei = TeiSiteObject.this.tei
    final override protected def headerSummary: Seq[Node] = TeiSiteObject.this.headerSummary
    final override protected def teiTransformer: Tei.Transformer = TeiSiteObject.this.teiTransformer

    final override protected def siteParameters: SiteParameters = site.siteParameters

    final override protected def resolver: LinkResolver = site.resolver(facsUrl)
  }

  protected def url: Seq[String]

  protected def viewer: Viewer

  protected def navigationLinks: Seq[NavigationLink] = Seq.empty

  protected def style: String = "main"

  protected def tei: Tei

  protected def headerSummary: Seq[Node] = Seq.empty

  // TODO generalize/conditionalize addCalendarDesc
  // TODO do not add header fields to TEI that is transformed into HTML...
  protected def teiTransformer: Tei.Transformer =
    Site.addPublicationStatement compose Site.addSourceDesc compose Tei.addLanguage

  def headTitle: Option[String] = None

  def title: Option[String] = None

  // TODO use - and handle - Option!
  def facsUrl: Seq[String] = null

  def simpleSubObjects: Seq[SimpleSiteObject]
}
