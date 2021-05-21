package org.opentorah.site

import org.opentorah.html
import org.opentorah.store.Caching
import org.opentorah.xml.Xml

trait HtmlContent[S <: Site[S]] {
  final def a(site: S): html.a = site.a(this)

  def htmlHeadTitle: Option[String]

  def htmlBodyTitle: Option[Xml.Nodes] = None

  // TODO make this class unaware of its position in the site's hierarchy:
  // inject positional data needed to construct the content into this method.
  def content(site: S): Caching.Parser[Xml.Element]
}
