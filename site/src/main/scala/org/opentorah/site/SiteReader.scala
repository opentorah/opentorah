package org.opentorah.site

import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.{Element, Parsable, Parser, Unparser}
import org.slf4j.{Logger, LoggerFactory}
import zio.Task
import java.io.File
import java.net.URL

// TODO add App/main functionality...  merge with Service!!!
abstract class SiteReader[S <: Site[S]] extends Element[S]("site") {
  final val logger: Logger = LoggerFactory.getLogger(this.getClass)

  final def readSite(url: URL): Task[S] = readSiteFile(Files.fileInDirectory(url, "site.xml"))

  final def readSiteFile(file: File): Parser[S] = parse(Files.file2url(file))

  final def readSiteFile(url: URL): Task[S] = Parser.toTask(
    Effects.effect(logger.info(s"Reading site from $url")) *>
    parse(url)
  )

  final def doReadSiteFile(file: File): S = Parser.run(readSiteFile(file))
}

object SiteReader {

  object Common extends SiteReader[Site.Common] {
    override def contentParsable: Parsable[Site.Common] = new Parsable[Site.Common] {
      override def parser: Parser[Site.Common] = for {
        fromUrl <- Element.currentFromUrl
        common <- SiteCommon.required()
      } yield new Site.Common(
        fromUrl,
        common
      )

      override def unparser: Unparser[Site.Common] = Unparser.concat[Site.Common](
        SiteCommon.required(_.common),
      )
    }
  }
}