package org.opentorah.site

import org.opentorah.html
import org.opentorah.xml.Xml
import zio.ZIO

trait HtmlContent[S <: Site[S]] {
  def viewer: Viewer
  def style: String = "/css/main"  // TODO `css` is always prepended; codify (and change to `asset`)
  def htmlHeadTitle: Option[String]
  def htmlBodyTitle: Option[Xml.Nodes] = None
  def navigationLinks(site: S): Caching.Parser[Seq[Xml.Element]] = ZIO.succeed(Seq.empty)
  def content        (site: S): Caching.Parser[    Xml.Element ]
  final def a        (site: S): html.a = site.a(this)
  def path           (site: S): Store.Path
}
