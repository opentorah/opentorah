package org.opentorah.archive.collector

import java.io.File
import org.opentorah.tei.Tei
import org.opentorah.util.Files
import org.opentorah.xml.{PaigesPrettyPrinter, XmlUtil}
import scala.xml.{Elem, Node}

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
  ): Unit = writeXml(
    file = new File(directory, fileName + ".xml"),
    elem
  )

  def writeXml(
    file: File,
    elem: Elem
  ): Unit = Files.write(
    file,
    content = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" + render(elem) + "\n"
  )

  def render(elem: Elem): String = new PaigesPrettyPrinter(
    width = 120,
    indent = 2,
    doNotStackElements = Set("choice"),
    nestElements = Set("p", /*"abstract",*/ "head", "salute", "dateline", "item"),
    clingyElements = Set("note", "lb", "sic", "corr")
  ).render(elem)

  // TODO move into Site
  def writeTei(
    directory: File,
    fileName: String,
    head: Option[Node], // TODO do not supply where not needed
    content: Seq[Node],
    style: Option[String] = None,
    target: String,
    yaml: Seq[(String, String)] = Seq.empty
  ): Unit = {
    val tei = Tei(
      publisher = <ptr target="www.alter-rebbe.org"/>,
      availabilityStatus = "free", availability =
        <licence>
          <ab>
            <ref n="license" target="http://creativecommons.org/licenses/by/4.0/">
              Creative Commons Attribution 4.0 International License</ref>
          </ab>
        </licence>,
      sourceDesc = <p>Facsimile</p>,
      body = head.fold[Seq[Node]](Seq.empty)(head => Seq(<head>{head}</head>)) ++ content
    )

    Util.writeXml(directory, fileName, Tei.toXml(tei))

    Util.writeTeiWrapper(
      directory,
      fileName,
      teiPrefix = None,
      style,
      target,
      yaml = head.fold[Seq[(String, String)]](Seq.empty)(head => Seq("title" -> Util.quote(XmlUtil.spacedText(head)))) ++ yaml
    )
  }
}
