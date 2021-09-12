package org.opentorah.util

import java.io.{BufferedWriter, File, FileWriter}
import java.net.{URI, URL, URLDecoder}
import org.slf4j.{Logger, LoggerFactory}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import scala.io.Source

object Files:
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def filesWithExtensions(directory: File, extension: String): Seq[String] =
    if !directory.exists then Seq.empty else for
      file <- directory.listFiles.toSeq
      result = nameAndExtension(file.getName)
      if result._2.contains(extension)
    yield result._1

  def dropAllowedExtension(nameWihtExtension: String, allowedExtension: String): String =
    val (name: String, extension: Option[String]) = nameAndExtension(nameWihtExtension)
    if extension.nonEmpty && !extension.contains(allowedExtension) then
      throw IllegalArgumentException(s"Extension must be '$allowedExtension' if present: $nameWihtExtension")
    name

  def nameAndExtension(fullName: String): (String, Option[String]) = Strings.split(fullName, '.')

  def prefixedDirectory(directory: File, prefix: Option[String]): File =
    prefix.fold(directory)(prefix => File(directory, prefix))

  def write(file: File, replace: Boolean, content: String): Unit =
    if !replace && file.exists then logger.debug(s"Already exists: $file")
    else write(file, content)

  def write(file: File, content: String): Unit =
    logger.debug(s"Writing $file")
    file.getParentFile.mkdirs()
    val writer: BufferedWriter = BufferedWriter(new FileWriter(file))
    try writer.write(content) finally writer.close()

  def read(file: File): Seq[String] =
    val source = Source.fromFile(file)
    // Note: toList materializes the iterator before closing the source
    val result = source.getLines().toList
    source.close
    result

  def readFile(file: File): Array[Byte] =
    java.nio.file.Files.readAllBytes(Paths.get(file.toURI))

  def deleteFiles(directory: File): Unit = if directory.exists() then
    if directory.isDirectory then for file <- directory.listFiles() do deleteFiles(file)
    directory.delete()

  def url2file(url: URL): File = Paths.get(url.toURI).toFile

  def file2url(file: File): URL = file.toURI.toURL

  def string2url(string: String): URL = URI.create(string).toURL

  def subdirectory(url: URL, subdirectoryName: String): URL = URL(url, subdirectoryName + "/")

  def fileInDirectory(url: URL, fileName: String): URL = URL(url, fileName)

  def pathUnder(url: URL, path: String): URL = URL(url, if path.startsWith("/") then path.drop(1) else path)

  //def getParent(url: URL): URL = new URL(url, "..")

  def subUrl(base: Option[URL], url: String): URL = base.fold(URL(url))(new URL(_, url))
  private def subUrl(base: URL, url: String): URL = URL(base, url)
  def subFile(base: URL, url: String): File = url2file(subUrl(base, url))

  def splitUrl(urlRaw: String): Seq[String] =
    val url: String = if urlRaw.isEmpty then "/" else urlRaw
    require(url.startsWith("/"))
    url.substring(1).split("/").toIndexedSeq.filterNot(_.isBlank)

  def splitAndDecodeUrl(url: String): Seq[String] = splitUrl(url).map(urlDecode)

  private def urlDecode(segment: String): String = URLDecoder.decode(segment, StandardCharsets.UTF_8)

  def mkUrl(segments: Seq[String]): String = segments.mkString("/", "/", "")

  @scala.annotation.tailrec
  def file(directory: File, segments: Seq[String]): File =
    if segments.isEmpty then directory
    else file(File(directory, segments.head), segments.tail)
