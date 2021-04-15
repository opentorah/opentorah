package org.opentorah.docbook

import org.opentorah.site.{Caching, Store}
import org.opentorah.util.Files
import org.opentorah.xml.{Catalog, Parser, PrettyPrinter, Resolver, Xml}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File

object ProcessDocBookDirect {
  private val logger: Logger = LoggerFactory.getLogger("org.opentorah.docbook.ProcessDocBookDirect")

  def main(args: Array[String]): Unit = {
    // Input arguments

    val root: File = new File(args(0))
    logger.warn(s"root: $root")

    val siteFile: File = new File(args(1))
    logger.warn(s"site: $siteFile")
    val site: DocBookSite = DocBookSite.read(siteFile)

    val inputDocuments: List[String] = args(2).split(",").toList
    logger.warn(s"inputDocuments: $inputDocuments")

    val substitutions: Map[String, String] = args(3).split(",").map { substitution =>
      val parts: Array[String] = substitution.split("=")
      parts.head -> parts.tail.mkString("=")
    }.toMap
    logger.warn(s"substitutions: $substitutions")

    val outputRoot: File = {
      val outputPath: String = args(4)
      if (outputPath.isEmpty) Layout.buildDirectory(root, "docBookDirect") else new File(outputPath)
    }
    logger.warn(s"output: $outputRoot")

    val dataGeneratorClass: Option[String] = if (args.length > 5) Some(args(5)) else None

    val layout: Layout = Layout.forRoot(
      root = root,
      catalogDirectoryOverride = Some(Layout.buildDirectory(root, "docBookCatalog")),
      outputRootOverride = Some(outputRoot)
    )

    // Generated data
    if (dataGeneratorClass.isEmpty)
      logger.warn("skipping DocBook data generation: 'dataGenerationClass' is not set") else {
      logger.warn(s"running DocBook data generator ${dataGeneratorClass.get} into ${layout.dataDirectory}")
      Class.forName(dataGeneratorClass.get)
        .getMethod("main", classOf[Array[String]])
        .invoke(null, Array(layout.dataDirectory.toString))
    }

    // Write XML catalog
    logger.warn(s"writing XML Catalog into ${layout.catalogFile}")
    Operations.writeDtd(layout.dtdFile, substitutions)
    Operations.writeCatalog(
      file = layout.catalogFile,
      replace = true,
      content =
        Seq(
          Catalog.nextCatalogSystem,
          Operations.dtdLink(layout.dtdFile)
        ) ++
        Operations.data(layout.dataDirectory)
    )

    // TODO enable when PrettyPrinter keeps empty lines and stacks mixed content.
    // Pretty-print the sources
//    logger.warn(s"Pretty-printing files in ${layout.inputDirectory}")
//    PrettyPrinter.prettyPrint(
//      roots = List(layout.inputDirectory),
//      f = element =>
//        if (Dom.getNamespace(element) == DocBook.namespace.default)
//          (DocBook.prettyPrinter, Some(DocBook.doctype(Dom.getName(element))))
//        else
//          (Store.prettyPrinter, None)
//    )

    // Wipe out output root
    logger.warn(s"deleting $outputRoot")
    Files.deleteFiles(outputRoot)

    // Process all documents
    val prefixed: Boolean = inputDocuments.length > 1
    val resolver: Resolver = new Resolver(layout.catalogFile)
    for (documentName: String <- inputDocuments) {
      val forDocument: Layout.ForDocument = layout.forDocument(prefixed, documentName)
      val inputFile: File = forDocument.inputFile
      val outputFile: File = new File(forDocument.outputDirectory(isIntermediate = false, name = "html"), "index.html")
      logger.warn(s"processing '$documentName'\n  from '$inputFile'\n  to   '$outputFile'.")
      val htmlContent: DocBookHtmlContent = new DocBookHtmlContent(inputFile, resolver)
      val content: String = Parser.run[String](Caching.provide(site.caching, site.renderHtmlContent(htmlContent)))
      Files.write(outputFile, content)
    }
  }
}
