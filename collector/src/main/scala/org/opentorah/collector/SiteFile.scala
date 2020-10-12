package org.opentorah.collector

import org.opentorah.tei.{Tei, Tei2Html}
import org.opentorah.xml.PrettyPrinter

import scala.xml.Elem

trait SiteFile {

  protected def siteObject: SiteObject

  def url: Seq[String]

  def viewer: Viewer

  final def content: String =
    "<!DOCTYPE html>\n" ++
    SiteFile.prettyPrinter.render(Html.defaultLayout(
      siteObject.site.siteParameters,
      new PageParameters(
        target = Some(viewer),
        style = if (siteObject.isWide) "wide" else "main",
        title = siteObject.title,
        navigationLinks = navigationLinks
      ),
      content = Tei2Html.transform(
        siteObject.site.resolver(siteObject.facsUrl),
        contentElement
      )
    ))

  protected def navigationLinks: Seq[NavigationLink]

  protected def contentElement: Elem
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
