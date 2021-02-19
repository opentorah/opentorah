package org.opentorah.collector

import org.opentorah.html
import org.opentorah.xml.Xml
import zio.ZIO

trait HtmlContent {
  def viewer: Viewer = Viewer.default
  def isWide: Boolean = false
  def htmlHeadTitle: Option[String]
  def htmlBodyTitle: Option[Xml.Nodes] = None

  final def a(site: Site): html.a = site.a(path(site))

  def path           (site: Site): Store.Path
  def navigationLinks(site: Site): Caching.Parser[Seq[Xml.Element]] = ZIO.succeed(Seq.empty)

  def content        (site: Site): Caching.Parser[Xml.Element]
}
