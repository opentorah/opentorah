package org.opentorah.store

import java.io.File
import java.net.URL
import org.opentorah.util.Files
import org.opentorah.xml.{Element, From, PaigesPrettyPrinter, Parser, Text, ToXml, XmlUtil}
import scala.xml.Elem

object FilesList extends Element[Seq[String]](
  elementName = "filesList",
  parser = Text("file").allMustBe
) with ToXml[Seq[String]] {

  override def toXml(value: Seq[String]): Elem =
    <filesList>
      {for (file <- value) yield <file>{file}</file>}
    </filesList>

  def filesWithExtensions(directory: URL, list: URL, extension: String): Seq[String] = {
    if (!Files.isFile(directory)) Parser.parseDo(FilesList.parse(From.url(list))) else {
      val result: Seq[String] = Files.filesWithExtensions(Files.url2file(directory), extension).sorted
      if (Files.isFile(list)) writeXml(toXml(result), Files.url2file(list))
      result
    }
  }

  private def writeXml(
    elem: Elem,
    file: File
  ): Unit = Files.write(
    file,
    content = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" +
    new PaigesPrettyPrinter().render(elem) +
    "\n"
  )
}
