package org.podval.judaica.importers

import java.io.File
import org.podval.judaica.viewer.{DivContent, Edition, Works}
import org.podval.judaica.parsers.ContentParser
import org.digitaljudaica.store.metadata.Xml

trait Importer {
  def importWork(inputDirectoryPath: String): Unit = {
    val inputDirectory = new File(inputDirectoryPath)
    val edition: Edition = Works.getWorkByName(workName).getEditionByName(editionName)
    for ((inputName, outputName) <- books) {
      val inputFile: File = new File(inputDirectory, inputName + "." + getInputExtension)
      val outputFile: File = new File(edition.directory, outputName + ".xml") //  .storage.storage(outputName).asFile.file
      val content: DivContent = parseBook(inputFile, outputName)
      val result: DivContent = processBook(content, edition, outputName)
      Xml.print(ContentParser.toXmlNode(result), outputFile)
    }
  }

  protected def workName: String

  protected def editionName: String

  protected def books: Map[String, String]

  protected def getInputExtension: String

  protected def parseBook(file: File, outputName: String): DivContent

  protected def processBook(content: DivContent, edition: Edition, outputName: String): DivContent = content
}
