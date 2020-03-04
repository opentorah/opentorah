package org.digitaljudaica.archive.collector

import java.io.File
import org.digitaljudaica.xml.Print
import org.digitaljudaica.util.Files
import scala.xml.Elem

object Util {

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

    Files.write(file, result.mkString("\n"))
  }

  def writeXml(
    directory: File,
    fileName: String,
    elem: Elem
  ): Unit = Files.write(
    file = new File(directory, fileName + ".xml"),
    content = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" + Print.render(elem) + "\n"
  )
}
