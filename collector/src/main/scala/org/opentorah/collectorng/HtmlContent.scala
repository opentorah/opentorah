package org.opentorah.collectorng

import org.opentorah.xml.Xml

trait HtmlContent {
  def viewer: Html.Viewer
  def isWide: Boolean
  def htmlTitle: Option[String]
  def navigationLinks: Seq[Html.NavigationLink]
  def lang: Option[String]
  def content(site: Site): Xml.Element
}
