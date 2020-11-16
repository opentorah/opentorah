package org.opentorah.collector

import org.opentorah.tei.Tei
import org.opentorah.xml.PrettyPrinter

// TODO add backlink to the SiteObject?
// TODO various indices and reports no longer have to be in TEI; rework them into HTML but apply resolver somehow?
trait SiteFile {

  final def content: String = {
    val titleAndContent: TitleAndContent = this.titleAndContent

    "<!DOCTYPE html>\n" ++
    SiteFile.prettyPrinter.render(Html.toHtml(
      siteParameters,
      pageParameters = new PageParameters(
        target = Some(viewer),
        style = style,
        // TODO extract headTitle from title, then...
        headTitle = None,
        title = titleAndContent.title,
        navigationLinks = navigationLinks
      ),
      content = titleAndContent.content
    ))
  }

  def url: Seq[String]

  def viewer: Viewer

  protected def siteParameters: SiteParameters

  protected def style: String

  protected def navigationLinks: Seq[NavigationLink]

  protected def titleAndContent: TitleAndContent
}

object SiteFile {
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
