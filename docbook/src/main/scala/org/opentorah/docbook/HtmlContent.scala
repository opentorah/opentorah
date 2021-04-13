package org.opentorah.docbook

import org.opentorah.site.Caching.Parser
import org.opentorah.site.{Store, Viewer}
import org.opentorah.util.Files
import org.opentorah.xml.{Dom, Resolver, Xml}
import zio.ZIO
import java.io.File

final class HtmlContent(
  inputFile: File,
  resolver: Resolver
) extends org.opentorah.site.HtmlContent[Site] {
  override def viewer: Viewer = Site.defaultViewer

  override def style: String = "/css/main"

  override def htmlHeadTitle: Option[String] = None

  override def content(site: Site): Parser[Xml.Element] = {
    // Scala XML does not work with XInclude-aware parsers (https://github.com/scala/scala-xml/issues/506),
    // but DocBook uses XInclude to assemble the document...
    val dom = Dom.loadFromUrl(Files.file2url(inputFile), resolver = Some(resolver))
    val string = DocBook.prettyPrinter.renderXml(dom)
    val result = Xml.loadFromString(string)
    ZIO.succeed(result) // TODO real Parser
  }

  override def path(site: Site): Store.Path = ???
}
