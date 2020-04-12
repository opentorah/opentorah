package org.opentorah.util

import java.io.{BufferedWriter, File, FileWriter}
import java.net.URL
import scala.io.Source

object Files {

  def filesWithExtensions(directory: File, extension: String): Seq[String] =
    if (!directory.exists) Seq.empty else for {
      file <- directory.listFiles.toSeq
      result = nameAndExtension(file.getName)
      if result._2.contains(extension)
    } yield result._1

  def pathAndName(path: String): (Option[String], String) = {
    val lastSlash: Int = path.lastIndexOf('/')
    if (lastSlash == -1) (None, path)
    else (Some(path.substring(lastSlash)), path.substring(lastSlash+1))
  }

  def nameAndExtension(fullName: String): (String, Option[String]) = {
    val dot: Int = fullName.lastIndexOf('.')
    if (dot == -1) (fullName, None)
    else (fullName.substring(0, dot), Some(fullName.substring(dot+1)))
  }

  def write(file: File, content: String): Unit = {
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

  def deleteFiles(directory: File): Unit = {
    directory.mkdirs()
    for (file <- directory.listFiles()) file.delete()
  }

  def url2file(url: URL): File = java.nio.file.Paths.get(url.toURI).toFile

  def file2url(file: File): URL = file.toURI.toURL

  def subdirectory(url: URL, subdirectoryName: String): URL = new URL(url, subdirectoryName + "/")

  def fileInDirectory(url: URL, fileName: String): URL = new URL(url, fileName)

  def isFile(url: URL): Boolean = url.getProtocol == "file"

  def removePart(from: String): String = {
    val sharp = from.indexOf('#')
    if (sharp == -1) from else from.substring(0, sharp)
  }

  def addPart(url: Seq[String], part: String): Seq[String] =
    url.init :+ (url.last + "#" + part)

  def mkUrl(segments: Seq[String]): String = segments.mkString("/", "/", "")

  @scala.annotation.tailrec
  def file(directory: File, segments: Seq[String]): File =
    if (segments.isEmpty) directory
    else file(new File(directory, segments.head), segments.tail)

  def spacesToUnderscores(what: String): String = what.replace(' ', '_')

  def underscoresToSpaces(what: String): String = what.replace('_', ' ')
}
