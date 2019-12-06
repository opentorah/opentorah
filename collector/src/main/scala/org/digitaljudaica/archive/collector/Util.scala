package org.digitaljudaica.archive.collector

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object Util {

  def filesWithExtensions(directory: File, extension: String): Seq[String] = {
    (if (!directory.exists) Seq.empty else directory.listFiles.toSeq)
      .map(_.getName)
      .filter(_.endsWith(extension)).map(_.dropRight(extension.length))
  }

  def htmlFile(directory: File, fileName: String): File = new File(directory, fileName + ".html")

  def writeTeiYaml(
    file: File,
    style: String,
    tei: String,
    title: String,
    target: String
  ): Unit = writeYaml(file, "tei", Seq(
    "style" -> style,
    "tei" -> tei,
    "title" -> title,
    "target" -> target
  ))

  def writeYaml(
    file: File,
    layout: String,
    yaml: Seq[(String, String)],
    content: Seq[String] = Seq.empty
  ): Unit = {
    val result: Seq[String] =
      Seq("---") ++
        (for ((name, value) <- ("layout", layout) +: yaml) yield name + ": " + value) ++
        Seq("---") ++
        Seq("") ++ content

    write(file, result.mkString("\n"))
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

  def removeConsecutiveDuplicates[T](seq: Seq[T]): Seq[T] = seq match {
    case Nil => Nil
    case x :: y :: xs if x == y => removeConsecutiveDuplicates(y :: xs)
    case x :: xs => x +: removeConsecutiveDuplicates(xs)
  }
}
