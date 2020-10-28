package org.opentorah.collector

import org.opentorah.tei.{Body, Tei, Tei2Html}
import org.opentorah.xml.PrettyPrinter
import scala.xml.{Elem, Node}

abstract class SiteObject(val site: Site) {

  final def content(siteFile: SiteFile): String =
    "<!DOCTYPE html>\n" ++
      SiteObject.prettyPrinter.render(Html.toHtml(
        site.siteParameters,
        new PageParameters(
          target = Some(siteFile.viewer),
          style = if (isWide) "wide" else "main",
          // TODO extract headTitle from TEI.titleStmt.title, then - title, then...
          headTitle = headTitle,
          title = title,
          navigationLinks = siteFile.navigationLinks
        ),
        content = Tei2Html.transform(
          site.resolver(facsUrl),
          siteFile.contentElement
        )
      ))

  val htmlFile: SiteFile = new SiteFile {
    override def viewer: Viewer = SiteObject.this.viewer

    override def url: Seq[String] = htmlUrl

    override def navigationLinks: Seq[NavigationLink] = SiteObject.this.navigationLinks

    override def contentElement: Elem = {
      val result = tei
      val withSummary = result.copy(text = result.text.copy(body = new Body.Value(headerSummary ++ result.body.xml)))
      Tei.toXmlElement(teiTransformer(withSummary))
    }
  }

  protected def htmlUrl: Seq[String]

  protected def tei: Tei

  protected def headerSummary: Seq[Node] = Seq.empty

  protected def viewer: Viewer

  // TODO generalize/conditionalize addCalendarDesc
  // TODO do not add header fields to TEI that is transformed into HTML...
  protected def teiTransformer: Tei.Transformer =
    Site.addPublicationStatement compose Site.addSourceDesc compose Tei.addLanguage

  def isWide: Boolean = false

  def headTitle: Option[String] = None

  def title: Option[String] = None

  protected def navigationLinks: Seq[NavigationLink] = Seq.empty

  // TODO use - and handle - Option!
  def facsUrl: Seq[String] = null

  def simpleSubObjects: Seq[SimpleSiteObject]
}

object SiteObject {

  private val prettyPrinter: PrettyPrinter = Tei.prettyPrinter.copy(
    alwaysStackElements = Tei.prettyPrinter.alwaysStackElements ++ Set("nav", "header", "main", "div"),
    // Note: empty elements are mis-processed by the browser (next element gets inserted inside the empty one!),
    // so I make sure there are no empty elements in the HTML:
    allowEmptyElements = false,
    // ... except, some elements are mis-processed when they *are* non-empty (e.g., <br>),
    // and in general, it's weird to expand the elements that are always empty:
    keepEmptyElements = Set("br", "meta", "link", "img", "data")
  )
}
