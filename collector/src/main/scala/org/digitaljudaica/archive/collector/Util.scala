package org.digitaljudaica.archive.collector

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source
import scala.xml.Node

object Util {

  def filesWithExtensions(directory: File, extension: String): Seq[String] = {
    (if (!directory.exists) Seq.empty else directory.listFiles.toSeq)
      .map(_.getName)
      .filter(_.endsWith(extension)).map(_.dropRight(extension.length))
  }

  def htmlFile(directory: File, fileName: String): File = new File(directory, fileName + ".html")

  def quote(what: String): String = s"'$what'"

  def writeTeiWrapper(
    directory: File,
    fileName: String,
    teiPrefix: Option[String] = None,
    style: Option[String] = None,
    target: String,
    yaml: Seq[(String, String)]
  ): Unit = writeWithYaml(
    file = htmlFile(directory, fileName),
    layout = "tei",
    yaml =
      style.fold[Seq[(String, String)]](Seq.empty)(style => Seq("style" -> style)) ++
      Seq(
        "tei" -> quote(teiPrefix.getOrElse("") + fileName + ".xml"),
        "target" -> target
      ) ++ yaml
  )

  def writeWithYaml(
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

  def writeTei(
    directory: File,
    fileName: String,
    head: Node,
    content: Seq[Node],
    style: Option[String] = None,
    target: String,
    yaml: Seq[(String, String)] = Seq.empty
  ): Unit = {
    val elem = Tei.tei(head, content)
    Util.write(
      file = new File(directory, fileName + ".xml"),
      content = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" +
        Xml.format(elem)
    )

    val title: String = Xml.spacedText(head)

    writeTeiWrapper(
      directory,
      fileName,
      teiPrefix = None,
      style,
      target,
      yaml = Seq("title" -> quote(title)) ++ yaml
    )
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

  def removeConsecutiveDuplicates[T](seq: Seq[T]): Seq[T] = removeConsecutiveDuplicates(Seq.empty, seq.toList)

  def removeConsecutiveDuplicates[T](result: Seq[T], seq: List[T]): Seq[T] = seq match {
    case x :: y :: xs =>
      removeConsecutiveDuplicates(
        if (x == y) result else result :+ x,
        y :: xs
      )
    case x :: Nil => result :+ x
    case Nil => result
  }
}
