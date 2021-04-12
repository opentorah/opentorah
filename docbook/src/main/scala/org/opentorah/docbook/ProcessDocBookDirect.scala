package org.opentorah.docbook

import org.opentorah.site.Caching
import org.opentorah.util.Files
import org.opentorah.xml.{Parser, Resolver}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File

object ProcessDocBookDirect {
  private val logger: Logger = LoggerFactory.getLogger("org.opentorah.docbook.ProcessDocBookDirect")

  def run(layout: Layout, siteFile: File, inputDocuments: List[(String, Boolean)], resolver: Resolver): Unit = {
    val site: Site = Site.read(siteFile)

    for ((documentName: String, prefixed: Boolean) <- inputDocuments) {
      val forDocument: Layout.ForDocument = layout.forDocument(prefixed, documentName)
      val inputFile: File = layout.inputFile(documentName)
      val outputDirectory: File = new File(layout.directOutputRoot, "html")
      val outputFile: File = new File(outputDirectory, "index.html")

      logger.warn(s"DocBook: direct processing '$documentName' from '$inputFile' to '$outputFile'.")
      val htmlContent = new HtmlContent(inputFile, resolver)
      Files.write(outputFile, Parser.run[String](Caching.provide(site.caching, site.renderHtmlContent(htmlContent))))
    }
  }

  def main(args: Array[String]): Unit = {
    val rootPath: String = args(0)
    val sitePath: String = args(1)
    val inputDocument: String = args(2)
    val dataGeneratorClass: Option[String] = if (args.length > 3) Some(args(3)) else None

    logger.warn(s"rootPath: $rootPath")
    logger.warn(s"sitePath: $sitePath")
    logger.warn(s"inputDocument: $inputDocument")
    logger.warn(s"dataGeneratorClass: $dataGeneratorClass")

    val layout: Layout = Layout.forRoot(new File(rootPath))

    if (dataGeneratorClass.isEmpty)
      logger.warn("Skipping DocBook data generation: 'dataGenerationClass' is not set") else {
      val dataDirectory: File = layout.dataDirectory
      logger.warn(s"Running DocBook data generator ${dataGeneratorClass.get} into $dataDirectory")
      Class.forName(dataGeneratorClass.get)
        .getMethod("main", classOf[Array[String]])
        .invoke(null, Array(dataDirectory.toString))
    }

    run(
      layout = layout,
      siteFile = new File(sitePath),
      inputDocuments = List((inputDocument, false)),
      resolver = Operations.writeCatalog(
        layout = layout,
        xslt1 = None,
        xslt2 = None,
        substitutions = Map.empty
      )
    )
  }
}
