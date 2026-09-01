package org.opentorah.util

import org.slf4j.{Logger, LoggerFactory}

import java.io.{BufferedWriter, File, FileWriter}
import java.net.{URI, URL, URLDecoder}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

object Files:
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def nameAndExtension(fullName: String): (String, Option[String]) = Strings.split(fullName, '.')

  def write(file: File, replace: Boolean, content: String): Unit =
    if !replace && file.exists then logger.debug(s"Already exists: $file")
    else write(file, content)

  def write(file: File, content: String): Unit =
    logger.debug(s"Writing $file")
    file.getParentFile.mkdirs()
    val writer: BufferedWriter = BufferedWriter(new FileWriter(file))
    try writer.write(content) finally writer.close()
  
  def fileInDirectory(url: URL, fileName: String): URL = subUrl(url, fileName)

  def subUrl(base: Option[URL], url: String): URL = base.fold(URI(url).toURL)(subUrl(_, url))

  private def subUrl(base: URL, url: String): URL = base.toURI.resolve(url).toURL
  
  def splitUrl(urlRaw: String): Seq[String] =
    val url: String = if urlRaw.isEmpty then "/" else urlRaw
    val startsWithSlash: Boolean = url.startsWith("/")
    // TODO? require(startsWithSlash)
    (if startsWithSlash then url.substring(1) else url).split("/").toIndexedSeq.filterNot(_.isBlank)

  def splitAndDecodeUrl(url: String): Seq[String] = splitUrl(url).map(urlDecode)

  private def urlDecode(segment: String): String = URLDecoder.decode(segment, StandardCharsets.UTF_8)
