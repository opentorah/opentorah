package org.opentorah.store

import org.opentorah.xml.{Element, Parsable, Parser, PrettyPrinter, Text}
import java.net.URL
import org.opentorah.util.Files

object FilesList extends Element[Seq[String]]("filesList") {

  override def contentParsable: Parsable[Seq[String]] = Text("file").seq

  def get(
    baseUrl: URL,
    directoryName: String,
    listName: Option[String],
    extension: String
  ): (Seq[String], String => URL) = {
    val directory: URL = Files.subdirectory(baseUrl, directoryName)
    val list: URL = Files.fileInDirectory(baseUrl, listName.getOrElse(directoryName + "-list-generated.xml"))

    val fileNames: Seq[String] = if (!Files.isFileUrl(directory)) Parser.parseDo(parse(list)) else {
      val result: Seq[String] = Files.filesWithExtensions(Files.url2file(directory), extension).sorted
      if (Files.isFileUrl(list)) Files.write(
        file = Files.url2file(list),
        content = PrettyPrinter.default.renderXml(required.xml(result))
      )
      result
    }

    (
      fileNames,
      (fileName: String) => Files.fileInDirectory(directory, fileName + "." + extension)
    )
  }
}
