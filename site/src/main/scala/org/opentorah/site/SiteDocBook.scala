package org.opentorah.site

import org.opentorah.docbook.DocBook
import org.opentorah.store.Caching
import org.opentorah.util.Files
import org.opentorah.xml.{Attribute, Catalog, Element, Parsable, Parser, Resolver, Unparser}
import java.io.File
import java.net.URL

final class SiteDocBook(
  val documents: Seq[String],
  val outputDirectory: String,
  val buildDirectory: String,
  val dataGeneratorClass: Option[String],
  val substitutions: Map[String, String]
) {
  private def logger = Site.logger

  def process[S <: Site[S]](site: Site[S]): Unit = {
    val baseUrl: URL = site.fromUrl.url
    val documentFiles: Seq[File] = documents.map(Files.subFile(baseUrl, _))
    val outputDirectoryFile: File = Files.subFile(baseUrl, outputDirectory)
    val buildDirectoryFile: File = Files.subFile(baseUrl, buildDirectory)

    logger.warn(
      "Processing DocBook:\n"                      ++
      s"  inputDocuments : $documentFiles\n"       ++
      s"  buildDirectory : $buildDirectoryFile\n"  ++
      s"  outputDirectory: $outputDirectoryFile\n" ++
      s"  substitutions  : $substitutions\n"
    )

    // layout
    val catalogDirectory: File = new File(buildDirectoryFile, "docBookCatalog")
    val dtdFile: File = new File(catalogDirectory, "substitutions.dtd")
    val catalogFile: File = new File(catalogDirectory, "catalog.xml")
    val dataDirectory: File = new File(buildDirectoryFile,"data")

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

    // Wipe out output root
    logger.warn(s"deleting $outputDirectoryFile")
    Files.deleteFiles(outputDirectoryFile)

    // Process all documents
    val prefixed: Boolean = documentFiles.length > 1
    val resolver: Resolver = new Resolver(catalogFile)

    for (documentFile: File <- documentFiles) {
      val documentName: String = Files.nameAndExtension(documentFile.getName)._1
      val outputFile: File = Files.file(outputDirectoryFile,
        (if (!prefixed) Seq.empty else Seq(documentName)) ++ Seq("html", "index.html"))
      logger.warn(s"processing '$documentName'\n  from '$documentFile'\n  to   '$outputFile'.")
      val htmlContent: DocBookHtmlContent[S] = new DocBookHtmlContent(documentFile, resolver)
      val content: String = Parser.run[String](Caching.provide(site.caching, site.renderHtmlContent(htmlContent)))
      Files.write(outputFile, content)
    }
  }

  def prettyPrint(): Unit = {
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
  }
}

object SiteDocBook extends Element[SiteDocBook]("docbook") {

  private val documentsAttribute: Attribute.Required[String] = Attribute("documents").required
  private val outputDirectoryAttribute: Attribute.Required[String] = Attribute("outputDirectory").required
  private val buildDirectoryAttribute: Attribute.Required[String] = Attribute("buildDirectory").required
  private val dataGeneratorClassAttribute: Attribute.Optional[String] = Attribute("dataGeneratorClass").optional

  override def contentParsable: Parsable[SiteDocBook] = new Parsable[SiteDocBook] {
    override def parser: Parser[SiteDocBook] = for {
      documents <- documentsAttribute().map(_.split(",").toList)
      outputDirectory <- outputDirectoryAttribute()
      buildDirectory <- buildDirectoryAttribute()
      dataGeneratorClass <- dataGeneratorClassAttribute()
    } yield new SiteDocBook(
      documents,
      outputDirectory,
      buildDirectory,
      dataGeneratorClass,
      substitutions = Map.empty // TODO
    )

    override def unparser: Unparser[SiteDocBook] = Unparser.concat[SiteDocBook](
      documentsAttribute(_.documents.mkString(",")),
      outputDirectoryAttribute(_.outputDirectory),
      buildDirectoryAttribute(_.buildDirectory),
      dataGeneratorClassAttribute(_.dataGeneratorClass)
    )
  }
}
