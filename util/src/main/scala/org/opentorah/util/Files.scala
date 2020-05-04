package org.opentorah.util

import java.io.{BufferedWriter, File, FileWriter}
import java.net.URL
import org.slf4j.{Logger, LoggerFactory}
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

  def pathAndName(path: String): (Option[String], String) = {
    val lastSlash: Int = path.lastIndexOf('/')
    if (lastSlash == -1) (None, path)
    else (Some(path.substring(lastSlash)), path.substring(lastSlash+1))
  }

  def nameAndExtension(fullName: String): (String, Option[String]) = split(fullName, '.')

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
    val result = source.getLines.toSeq
    source.close
    result
  }

  // TODO read eagerly, so that mkString() could be done on the outside:
  def read1(file: File): String = {
    val source = Source.fromFile(file)
    val result = source.getLines.toSeq.mkString("\n")
    source.close
    result
  }

  def deleteFiles(directory: File): Unit = if (directory.exists()) {
    if (directory.isDirectory) for (file <- directory.listFiles()) deleteFiles(file)
    directory.delete()
  }

  def url2file(url: URL): File = java.nio.file.Paths.get(url.toURI).toFile

  def file2url(file: File): URL = file.toURI.toURL

  def subdirectory(url: URL, subdirectoryName: String): URL = new URL(url, subdirectoryName + "/")

  def fileInDirectory(url: URL, fileName: String): URL = new URL(url, fileName)

  def isFile(url: URL): Boolean = url.getProtocol == "file"

  def urlAndPart(what: String): (String, Option[String]) = split(what, '#')

  def removePart(from: String): String = urlAndPart(from)._1

  def addPart(url: Seq[String], part: Option[String]): Seq[String] =
    part.fold(url){ part => addPart(url, part) }

  def addPart(url: Seq[String], part: String): Seq[String] =
    url.init :+ (url.last + "#" + part)

  def split(what: String, on: Char): (String, Option[String]) = {
    val index: Int = what.lastIndexOf(on)
    if (index == -1) (what, None)
    else (what.substring(0, index), Some(what.substring(index+1)))
  }

  def mkUrl(segments: Seq[String]): String = segments.mkString("/", "/", "")

  @scala.annotation.tailrec
  def file(directory: File, segments: Seq[String]): File =
    if (segments.isEmpty) directory
    else file(new File(directory, segments.head), segments.tail)

  def spacesToUnderscores(what: String): String = what.replace(' ', '_')

  def underscoresToSpaces(what: String): String = what.replace('_', ' ')
}
