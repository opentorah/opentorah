package org.digitaljudaica.util

import java.io.{BufferedWriter, File, FileWriter}
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

  def splice(file: File, start: String, end: String, what: Seq[String]): Unit = {
    println(s"Splicing ${file.getName}.")
    write(file, splice(read(file), start, end, what).mkString("\n"))
  }

  private def splice(lines: Seq[String], start: String, end: String, what: Seq[String]): Seq[String] = {
    val (prefix, tail) = lines.span(_ != start)
    if (tail.isEmpty) lines else {
      val (_, suffix) = tail.tail.span(_ != end)
      if (suffix.isEmpty) lines else {
        prefix ++ Seq(start) ++ what ++ suffix
      }
    }
  }

  def deleteFiles(directory: File): Unit = {
    directory.mkdirs()
    for (file <- directory.listFiles()) file.delete()
  }
}
