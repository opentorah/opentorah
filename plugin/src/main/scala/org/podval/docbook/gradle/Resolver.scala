package org.podval.docbook.gradle

import java.io.{File, StringReader}
import java.net.URI
import javax.xml.transform.{Source, URIResolver}
import javax.xml.transform.stream.StreamSource
import org.xml.sax.{EntityResolver, InputSource}

final class Resolver(
 docBookXslDirectory: File,
 entities: Map[String, String],
 dataDirectory: File,
 logger: Logger
) extends URIResolver with EntityResolver {

  logger.info(
    s"""Resolver(
       |  entities = $entities,
       |  docBookXslDirectory = "$docBookXslDirectory",
       |  dataDirectory = "$dataDirectory"
       |)""".stripMargin
  )

  private val parentResolver: org.xmlresolver.Resolver = Resolver.createParentResolver

  override def resolve(href: String, base: String): Source = {
    val uri: URI = new URI(base).resolve(new URI(href))
    val parameters: String = s"Resolver.resolve(href=$href, base=$base)\n  $uri\n"

    Resolver.resolveFile[Source](
      parameters = parameters,
      uri = uri,
      directoryUris = Resolver.docBookXslUris,
      directory = docBookXslDirectory,
      packer = Resolver.fileSource,
      logger = logger
    ).orElse {
      val result: Option[Source] = Option(parentResolver.resolve(href, base))
      if (result.isDefined) logger.info(s"$parameters  parent resolved to: ${result.get.getSystemId}")
      result
    }.getOrElse {
      logger.error(s"$parameters  unresolved")
      null
    }
  }

  override def resolveEntity(publicId: String, systemId: String): InputSource = {
    val parameters: String = s"EntityResolver.resolveEntity(publicId=$publicId, systemId=$systemId)\n"

    {
      if (publicId == Resolver.docBookPublicId) {
        val result: InputSource = Resolver.stringInputSource(Resolver.dtd(entities), Some(publicId), None)
        logger.info(s"$parameters substituting $publicId with $entities")
        Some(result)
      } else None
    }.orElse(Resolver.resolveFile[InputSource](
      parameters = parameters,
      uri = new URI(systemId),
      directoryUris = Resolver.dataUris,
      directory = dataDirectory,
      packer = Resolver.fileInputSource,
      logger = logger
    )).orElse {
      val result: Option[InputSource] = Option(parentResolver.resolveEntity(publicId, systemId))
      if (result.isDefined) logger.info(s"$parameters  parent resolved to: ${result.get.getSystemId}")
      result
    }.getOrElse {
      logger.error(s"$parameters  unresolved!")
      null
    }
  }
}

object Resolver {

  // If used in DocBook files, those point to the DocBook XSL files.
  val docBookXslUris: Seq[String] = Seq(
    "http://docbook.sourceforge.net/release/xsl-ns/current/",
    "https://cdn.docbook.org/release/latest/xslt/base/",
    "urn:docbook:xsl/"
  )

  val docBookPublicId: String = "-//OASIS//DTD DocBook XML V5.0//EN"

  // If used in DocBook files, those point to the data directory.
  val dataUris: Seq[String] = Seq(
    "http://podval.org/docbook/data/",
    "urn:docbook:data:/",
    "data:"
  )

  def dtd(entities: Map[String, String]): String = entities.toSeq.map {
    case (name: String, value: String) => s"""<!ENTITY $name "$value">"""
  }.mkString("", "\n", "\n")

  def resolveFile[R](
    parameters: String,
    uri: URI,
    directoryUris: Seq[String],
    directory: File,
    packer: File => R,
    logger: Logger
  ): Option[R] = {
    val result: Option[(String, File)] =
      if (uri.getScheme == "file") Some {
        (s"  file", new File(uri.getPath))
      } else Util.drop(directoryUris, uri.toString).map { path =>
        val file: File = new File(directory, path)
        (s"  resolved to $file", file)
      }

    result
      .flatMap[R] { case (message: String, file: File) =>
      val prefix: String = s"$parameters$message"
      if (!file.exists) {
        logger.error(s"$prefix\n  which does not exist!")
        None
      } else if (file.isDirectory) {
        logger.info(s"$prefix\n  which is a directory!")
        Some[R](null.asInstanceOf[R]) // suppress further resolution
      } else {
        logger.info(s"$prefix")
        Some(packer(file))
      }
    }
  }

  def fileInputSource(file: File): InputSource =
    new InputSource(file.toURI.toASCIIString)

  def stringInputSource(input: String, publicId: Option[String], systemId: Option[String]): InputSource = {
    val result = new InputSource(new StringReader(input))
    result.setPublicId(publicId.orNull)
    result.setSystemId(systemId.orNull)
    result
  }

  def resourceInputSource(name: String): InputSource = {
    val result = new InputSource(getClass.getResourceAsStream(name))
    result.setSystemId(name)
    result.setPublicId(null)
    result
  }

  def fileSource(file: File): Source =
    new StreamSource(file)

  def stringSource(input: String, publicId: Option[String], systemId: Option[String]): Source = {
    val result = new StreamSource(new StringReader(input))
    result.setPublicId(publicId.orNull)
    result.setSystemId(systemId.orNull)
    result
  }

  def createParentResolver: org.xmlresolver.Resolver = {
    val properties = new java.util.Properties
    properties.setProperty("cacheUnderHome", "yes")
    val configuration = new org.xmlresolver.Configuration(properties, null)
    val catalog = new org.xmlresolver.Catalog(configuration, "/etc/xml/catalog")
    new org.xmlresolver.Resolver(catalog)
  }
}
