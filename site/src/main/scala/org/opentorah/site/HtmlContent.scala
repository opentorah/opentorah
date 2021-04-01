package org.opentorah.site

import org.opentorah.xml.Xml
import zio.ZIO

trait HtmlContent[S <: Site[S]] {
  def viewer: Viewer
  def style: String
  def htmlHeadTitle: Option[String]
  def htmlBodyTitle: Option[Xml.Nodes] = None
  def navigationLinks(site: S): Caching.Parser[Seq[Xml.Element]] = ZIO.succeed(Seq.empty)
  def content        (site: S): Caching.Parser[    Xml.Element ]
}
