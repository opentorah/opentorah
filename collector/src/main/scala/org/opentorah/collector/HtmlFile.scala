package org.opentorah.collector

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
  private val prettyPrinter: PrettyPrinter = PrettyPrinter(
    alwaysStackElements = Set("nav", "header", "main", "div")
  )
}
