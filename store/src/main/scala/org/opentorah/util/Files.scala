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

  // TODO rename relativeUrl()
  def fileInDirectory(url: URL, fileName: String): URL = new URL(url, fileName)

  def isFile(url: URL): Boolean = url.getProtocol == "file"
}
