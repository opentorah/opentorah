package org.opentorah.site

import org.opentorah.docbook.DocBook
import org.opentorah.store.{Path, Store}
import org.opentorah.xml.{Parser, Resolver, ScalaXml}
import zio.ZIO
import java.io.File

// TODO dissolve into Site: introduce [Pre]Content and subsume this and Markdown into it.
final class DocBookHtmlContent[S <: Site[S]](
  inputFile: File,
  resolver: Resolver
) extends org.opentorah.site.HtmlContent.DefaultViewer[S]:

  override def htmlHeadTitle: Option[String] = None // TODO

  // TODO Caching.Parser?
  override def content(path: Path, site: S): Parser[ScalaXml.Element] =
    ZIO.succeed(DocBook.loadFromFile(inputFile, Some(resolver))) // TODO real Parser
