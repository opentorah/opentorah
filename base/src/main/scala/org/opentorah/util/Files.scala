package org.opentorah.util

import java.io.{BufferedWriter, File, FileWriter}
import java.net.{URL, URLDecoder}
import org.slf4j.{Logger, LoggerFactory}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import scala.io.Source

object Files {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def filesWithExtensions(directory: File, extension: String): Seq[String] =
    if (!directory.exists) Seq.empty else for {
      file <- directory.listFiles.toSeq
      result = nameAndExtension(file.getName)
      if result._2.contains(extension)
    } yield result._1

  def dropAllowedExtension(nameWihtExtension: String, allowedExtension: String): String = {
    val (name: String, extension: Option[String]) = nameAndExtension(nameWihtExtension)
    if (extension.nonEmpty && !extension.contains(allowedExtension))
      throw new IllegalArgumentException(s"Extension must be '$allowedExtension' if present: $nameWihtExtension")
    name
  }

  def nameAndExtension(fullName: String): (String, Option[String]) = Strings.split(fullName, '.')

  def prefixedDirectory(directory: File, prefix: Option[String]): File =
    prefix.fold(directory)(prefix => new File(directory, prefix))

  def write(file: File, replace: Boolean, content: String): Unit =
    if (!replace && file.exists) logger.debug(s"Already exists: $file")
    else write(file, content)

  def write(file: File, content: String): Unit = {
    logger.debug(s"Writing $file")
    file.getParentFile.mkdirs()
    val writer: BufferedWriter = new BufferedWriter(new FileWriter(file))
    try {
      writer.write(content)
    } finally {
      writer.close()
    }
  }

  def read(file: File): Seq[String] = {
    val source = Source.fromFile(file)
    // Note: toList materializes the iterator before closing the source
    val result = source.getLines().toList
    source.close
    result
  }

  def readFile(file: File): Array[Byte] =
    java.nio.file.Files.readAllBytes(Paths.get(file.toURI))

  def deleteFiles(directory: File): Unit = if (directory.exists()) {
    if (directory.isDirectory) for (file <- directory.listFiles()) deleteFiles(file)
    directory.delete()
  }

  def url2file(url: URL): File = Paths.get(url.toURI).toFile

  def file2url(file: File): URL = file.toURI.toURL

  def subdirectory(url: URL, subdirectoryName: String): URL = new URL(url, subdirectoryName + "/")

  def fileInDirectory(url: URL, fileName: String): URL = new URL(url, fileName)

  //def getParent(url: URL): URL = new URL(url, "..")

  def subUrl(base: Option[URL], url: String): URL = base.fold(new URL(url))(new URL(_, url))
  private def subUrl(base: URL, url: String): URL = new URL(base, url)
  def subFile(base: URL, url: String): File = url2file(subUrl(base, url))

  def splitUrl(url: String): Seq[String] = {
    require(url.startsWith("/"))
    url.substring(1).split("/").toIndexedSeq.filterNot(_.isBlank)
  }

  def splitAndDecodeUrl(url: String): Seq[String] = splitUrl(url).map(urlDecode)

  private def urlDecode(segment: String): String = URLDecoder.decode(segment, StandardCharsets.UTF_8)

  def mkUrl(segments: Seq[String]): String = segments.mkString("/", "/", "")

  @scala.annotation.tailrec
  def file(directory: File, segments: Seq[String]): File =
    if (segments.isEmpty) directory
    else file(new File(directory, segments.head), segments.tail)
}
