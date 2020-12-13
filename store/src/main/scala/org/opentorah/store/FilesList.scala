package org.opentorah.store

import org.opentorah.xml.{Antiparser, Element, Parser, PrettyPrinter, Text}
import java.net.URL
import org.opentorah.util.Files

object FilesList extends Element.WithToXml[Seq[String]]("filesList") {

  override protected def parser: Parser[Seq[String]] = Text("file").all

  override protected def antiparser: Antiparser[Seq[String]] =
    Antiparser.xml.compose[Seq[String]](value => for (file <- value) yield <file>{file}</file>)

  def get(
    baseUrl: URL,
    directoryName: String,
    listName: Option[String],
    extension: String
  ): (Seq[String], String => URL) = {
    val directory: URL = Files.subdirectory(baseUrl, directoryName)
    val list: URL = Files.fileInDirectory(baseUrl, listName.getOrElse(directoryName + "-list-generated.xml"))

    val fileNames: Seq[String] = if (!Files.isFile(directory)) Parser.parseDo(parse(list)) else {
      val result: Seq[String] = Files.filesWithExtensions(Files.url2file(directory), extension).sorted
      if (Files.isFile(list)) Files.write(
        file = Files.url2file(list),
        content = PrettyPrinter.default.renderXml(toXmlElement(result))
      )
      result
    }

    (
      fileNames,
      (fileName: String) => Files.fileInDirectory(directory, fileName + "." + extension)
    )
  }
}
