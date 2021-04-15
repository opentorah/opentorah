package org.opentorah.docbook

import org.opentorah.site.Caching.Parser
import org.opentorah.xml.{Resolver, Xml}
import zio.ZIO
import java.io.File

final class DocBookHtmlContent(
  inputFile: File,
  resolver: Resolver
) extends org.opentorah.site.HtmlContent[DocBookSite] {

  override def htmlHeadTitle: Option[String] = None // TODO

  override def content(site: DocBookSite): Parser[Xml.Element] =
    ZIO.succeed(DocBook.loadFromFile(inputFile, Some(resolver))) // TODO real Parser
}
