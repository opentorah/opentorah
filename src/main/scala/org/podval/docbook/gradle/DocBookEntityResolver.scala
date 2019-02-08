package org.podval.docbook.gradle

import java.io.File
import java.net.URI

import org.xml.sax.{EntityResolver, InputSource}

/**
  * Resolves references to data in DocBook files.
  *
  * @param dataDirectory
  * @param logger
  */
final class DocBookEntityResolver(
  entities: Map[String, String],
  dataDirectory: File,
  logger: Logger
) extends EntityResolver {

  override def resolveEntity(publicId: String, systemId: String): InputSource = {
    logger.info(s"EntityResolver.resolveEntity(publicId=$publicId, systemId=$systemId)")

    resolvePublicId(publicId).orElse(resolveSystemId(systemId)).getOrElse {
      logger.error(s"  unresolved: (publicId=$publicId, systemId=$systemId)")
      null
    }
  }

  private def resolvePublicId(publicId: String): Option[InputSource] = {
    if (publicId == DocBookEntityResolver.docBookPublicId) {
      val result = new EntitiesInputSource(entities)
      result.setPublicId(publicId)
      logger.info(s" substituting $publicId with $entities")
      Some(result)
    } else None
  }

  private def resolveSystemId(systemId: String): Option[InputSource] = {
    val uri: URI = new URI(systemId)

    val file: Option[File] =
      if (uri.getScheme == "file") {
        val result: File = new File(uri.getPath)
        logger.info(s"  file $result")
        Some(result)
      } else {
        Util.drop(DocBookEntityResolver.data, systemId).map { path =>
          val result: File = new File(dataDirectory, path)
          logger.info(s" found $result")
          result
        }
      }

    file.map(file => new InputSource(file.toURI.toASCIIString))
  }
}

object DocBookEntityResolver {
  val docBookPublicId: String = "-//OASIS//DTD DocBook XML V5.0//EN"

  // If used in DocBook files, those point to the data directory.
  val data: Seq[String] = Seq(
    "http://podval.org/docbook/data/",
    "urn:docbook:data:/",
    "data:"
  )
}
