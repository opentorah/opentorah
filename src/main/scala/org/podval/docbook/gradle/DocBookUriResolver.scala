package org.podval.docbook.gradle

import java.io.File
import java.net.URI
import javax.xml.transform.{Source, URIResolver}
import javax.xml.transform.stream.StreamSource

/**
  * Resolves references to DocBook XSL
  *
  * @param docBookXslDirectory
  * @param logger
  */
final class DocBookUriResolver(docBookXslDirectory: File, logger: Logger) extends URIResolver {

  override def resolve(href: String, base: String): Source = {
    logger.info(s"URIResolver.resolve(href=$href, base=$base)")
    resolve(new URI(base).resolve(new URI(href)))
  }

  def resolve(uri: URI): Source = {
    logger.info(s"URIResolver.resolve(uri=$uri)")

    val file: Option[File] =
      if (uri.getScheme == "file") {
        val result: File = new File(uri.getPath)
        logger.info(s"  file $result")
        Some(result)
      } else {
        Util.drop(DocBookUriResolver.docBookXsl, uri.toString).map { path =>
          val result: File = new File(docBookXslDirectory, path)
          logger.info(s" found $result")
          result
        }
      }

    file.fold[Source] {
      logger.error(s"  unresolved")
      null
    }{ file =>
      if (!file.exists()) {
        logger.error(s"  does not exist: $file")
        null
      } else if(file.isDirectory) {
        logger.info(s"  is a directory: $file")
        null
      } else {
        new StreamSource(file)
      }
    }
  }
}

object DocBookUriResolver {
  // If used in DocBook files, those point to the DocBook XSL files.
  val docBookXsl: Seq[String] = Seq(
    "http://podval.org/docbook/xsl/",
    "http://docbook.sourceforge.net/release/xsl-ns/current/"
  )
}
