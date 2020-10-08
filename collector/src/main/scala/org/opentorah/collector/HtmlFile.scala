package org.opentorah.collector

import org.opentorah.tei.Tei
import org.opentorah.xml.PrettyPrinter
import scala.xml.Elem

trait HtmlFile extends SiteFile {

  def viewer: Viewer

  final override def content: String =
    "<!DOCTYPE html>\n" ++
    HtmlFile.prettyPrinter.render(Html.defaultLayout(
      siteParameters,
      pageParameters,
      content = contentElement
    ))

  protected def siteParameters: SiteParameters

  protected def contentElement: Elem

  protected def pageParameters: PageParameters
}

object HtmlFile {
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
