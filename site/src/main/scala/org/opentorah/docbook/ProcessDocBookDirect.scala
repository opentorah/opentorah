package org.opentorah.docbook

import org.opentorah.site.{DocBookHtmlContent, Site, SiteReader}
import org.opentorah.store.Caching
import org.opentorah.util.Files
import org.opentorah.xml.{Catalog, Parser, Resolver}
import org.slf4j.{Logger, LoggerFactory}
import java.io.File

object ProcessDocBookDirect {
  private val logger: Logger = LoggerFactory.getLogger("org.opentorah.docbook.ProcessDocBookDirect")

  def main(args: Array[String]): Unit = run(
    site = SiteReader.Common.doReadSiteFile(new File(args(1))),
    inputDocuments = args(2).split(",").toList.map(new File(_)),
    outputDirectory = new File(args(4)),
    buildDirectory = new File(args(0)),
    substitutions =  args(3).split(",").map { substitution =>
      val parts: Array[String] = substitution.split("=")
      parts.head -> parts.tail.mkString("=")
    }.toMap,
    dataGeneratorClass = if (args.length > 5) Some(args(5)) else None
  )

  def run(
    site: Site.Common,
    inputDocuments: List[File],
    outputDirectory: File,
    buildDirectory: File,
    substitutions: Map[String, String],
    dataGeneratorClass: Option[String]
  ): Unit = {
    // log
    logger.warn(s"inputDocuments : $inputDocuments" )
    logger.warn(s"buildDirectory : $buildDirectory" )
    logger.warn(s"substitutions  : $substitutions"  )
    logger.warn(s"outputDirectory: $outputDirectory")

    // layout
    val catalogDirectory: File = new File(buildDirectory, "docBookCatalog")
    val dtdFile: File = new File(catalogDirectory, "substitutions.dtd")
    val catalogFile: File = new File(catalogDirectory, "catalog.xml")
    val dataDirectory: File = new File(buildDirectory,"data")

    // Generated data
    if (dataGeneratorClass.isEmpty)
      logger.warn("skipping DocBook data generation: 'dataGenerationClass' is not set") else {
      logger.warn(s"running DocBook data generator ${dataGeneratorClass.get} into $dataDirectory")
      Class.forName(dataGeneratorClass.get)
        .getMethod("main", classOf[Array[String]])
        .invoke(null, Array(dataDirectory.toString))
    }

    // Write XML catalog
    logger.warn(s"writing XML Catalog into $catalogFile")
    DocBook.writeDtd(dtdFile, substitutions)
    Catalog.write(
      file = catalogFile,
      replace = true,
      content =
        Seq(
          Catalog.nextCatalogSystem,
          DocBook.dtdLink(dtdFile)
        ) ++
          DocBook.data(dataDirectory)
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
    logger.warn(s"deleting $outputDirectory")
    Files.deleteFiles(outputDirectory)

    // Process all documents
    val prefixed: Boolean = inputDocuments.length > 1
    val resolver: Resolver = new Resolver(catalogFile)

    for (documentFile: File <- inputDocuments) {
      val documentName: String = Files.nameAndExtension(documentFile.getName)._1
      val outputFile: File = Files.file(outputDirectory,
        (if (!prefixed) Seq.empty else Seq(documentName)) ++ Seq("html", "index.html"))
      logger.warn(s"processing '$documentName'\n  from '$documentFile'\n  to   '$outputFile'.")
      val htmlContent: DocBookHtmlContent[Site.Common] = new DocBookHtmlContent(documentFile, resolver)
      val content: String = Parser.run[String](Caching.provide(site.caching, site.renderHtmlContent(htmlContent)))
      Files.write(outputFile, content)
    }
  }
}
