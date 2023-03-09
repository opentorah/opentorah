package org.opentorah.docbook

import org.opentorah.build.{BuildContext, Dependency, InstallableDependency}
import org.opentorah.fop.FopFonts
import org.opentorah.math.MathConfiguration
import org.opentorah.util.Files
import org.opentorah.xml.{Catalog, Dom, Resolver, Sax, ScalaXml, Xsl}

import java.io.File

// TODO combine epubEmbeddedFonts, xslt1version and xslt2version?
// TODO extract PDF author/title from the document itself
// TODO if I really need additional attributes - generalize Xsl.stylesheet() to take them:
//    xmlns:db={DocBook.namespace.uri}
//    exclude-result-prefixes="db"
final class DocBookProcessor(
  layout: Layout,
  context: BuildContext,
  outputDefault: Set[Variant],
  mathDefault: Option[MathConfiguration],
  substitutionsDefault: Map[String, String],
  xslt1versionDefault: Option[String],
  xslt2versionDefault: Option[String],
  epubEmbeddedFontsDefault: List[String],
  documents: Set[Document],
  common2parameters: Map[Common, Map[String, String]],
  variants: Set[Variant]
):

  private def inputFile(document: Document): File = Files.file(layout.input, s"${document.name}.xml")

  def prettyPrintDocBook: Seq[File] =
    val result: Seq[Seq[File]] = for
      document: Document <- documents.toSeq
      file = inputFile(document)
      if file.exists
    yield
      // TODO Scala XML does not work with XInclude-aware parsers (see https://github.com/scala/scala-xml/issues/506)
      val resolver: Resolver = DocBookProcessor.getResolver(
        layout,
        context,
        document.name,
        into = layout.documentTmp(document.name),
        xsltConfiguration = None
      )
      val dom: Dom.Element = Dom.load(Sax.file2inputSource(file), resolver = Some(resolver))
      val bases: Seq[String] = Dom.allBases(dom).filterNot(_.contains(":"))
      val includes: Seq[File] = for base: String <- bases yield File(file.getParentFile, base)
      file +: includes
    result.flatten

  def verify(): Unit =
    def logUnused[T <: HasName](whats: String, unused: Set[T]): Unit = if unused.nonEmpty then
      val unusedString: String = unused.map(_.name).mkString(", ")
      context.warn(s"DocBook: unused $whats: $unusedString")

    val variantsNonEmpty: Set[Variant] = variants.filterNot(_.isEmpty)
    val variantsUsed: Set[Variant] = documents.flatMap(_.output)
    logUnused("variants", variantsNonEmpty -- variantsUsed)

    val formats: Set[Format] = variantsNonEmpty.map(_.format)
    val formatsUsed: Set[Format] = variantsUsed.map(_.format)
    logUnused("formats", formats -- formatsUsed)

    val commons: Set[Common] = common2parameters.keySet
    val commonsUsed: Set[Common] = formatsUsed.flatMap(_.common)
    logUnused("common sections", commons -- commonsUsed)

    val documentsThatExist: Set[Document] = documents.flatMap((document: Document) =>
      val file: File = inputFile(document)
      if file.exists then Some(document) else
        context.warn(s"DocBook: input file for document ${document.name} does not exist: $file")
        None
    )
    if documentsThatExist.isEmpty then context.warn("DocBook: no documents configured to process")

    if outputDefault.isEmpty then logUnused("documents", documents.filter(_.output.isEmpty))

  def installDistributions(): Unit =
    for distribution: InstallableDependency.WithVersion[_] <- distributionsNeeded do
      distribution.getInstallation(context, installIfDoesNotExist = true, mustExist = true)

  def distributionsNeeded: Set[InstallableDependency.WithVersion[_]] = (
    for
      document: Document <- documents
      if inputFile(document).exists
      variantProcessor: VariantProcessor <- variantProcessors(document)
    yield
      variantProcessor.distributionsNeeded
    ).flatten

  def variantProcessors(document: Document): Set[VariantProcessor] =
    val output: Set[Variant] = if document.output.nonEmpty then document.output else outputDefault
    val formats: Seq[Format] = output.toSeq.map(_.format)

    for variant: Variant <- output yield
      val format: Format = variant.format
      // when there is only one variant with an underlying Format, output is named after the Format, not the variant:
      val outputName: String = if formats.count(_ == variant.format) > 1 then variant.name else format.name

      val parameters: Parameters = Parameters(
        format.common.flatMap((common: Common) => Seq(
          (common    .fullName + " defaults", common.parameters),
          (common    .fullName              , common2parameters.getOrElse(common, Map.empty))
        )) ++ Seq(
          (format    .name + " defaults"    , format.parameters),
          (format    .name                  , variant.parameters),
          (variant   .name                  , variant.configuration.map(_.parameters).getOrElse(Map.empty)),
          ("document" + document.name       , document.parameters)
        )
      )

      val math: MathConfiguration = Seq(
        document.math,
        variant.configuration.flatMap(_.math),
        variant.math,
        mathDefault,
        Some(MathConfiguration.default)
      )
        .flatten
        .reduce(_.orElse(_))

      val epubEmbeddedFonts: List[String] = Seq(
        document.epubEmbeddedFonts,
        variant.configuration.map(_.epubEmbeddedFonts).getOrElse(List.empty),
        variant.epubEmbeddedFonts,
        epubEmbeddedFontsDefault
      )
        .reduce((a, b) => if a.nonEmpty then a else b)

      val epubEmbeddedFontsString: Option[String] = if epubEmbeddedFonts.isEmpty then None else Some(
        FopFonts.getFiles(layout.fopConfigurationFile, epubEmbeddedFonts)
          .map(uri => File(uri.getPath).getAbsolutePath)
          .mkString(", ")
      )

      val xslt: Option[InstallableDependency.WithVersion[File]] = format match
        case xsltFormat: XsltFormat =>

          // XSLT
          val xslt1version: String = Seq(
            document.xslt1version,
            variant.configuration.flatMap(_.xslt1version),
            variant.xslt1version,
            xslt1versionDefault
          )
            .reduce(_.orElse(_))
            .getOrElse(Xslt.xslt1VersionDefault)

          val xslt2version: String = Seq(
            document.xslt2version,
            variant.configuration.flatMap(_.xslt1version),
            variant.xslt2version,
            xslt1versionDefault
          )
            .reduce(_.orElse(_))
            .getOrElse(Xslt.xslt2VersionDefault)

          Some(
            if xsltFormat.usesDocBookXslt2
            then Xslt.Xslt2(xslt2version)
            else Xslt.Xslt1(xslt1version)
          )
        case _ => None

      VariantProcessor(
        documentName = document.name,
        inputFile = inputFile(document),
        imagesDirectory = document.imagesDirectory,
        variant = variant,
        outputName = outputName,
        parameters = parameters,
        mathConfiguration = math,
        epubEmbeddedFontsString = epubEmbeddedFontsString,
        xslt = xslt
      )

  def process(globalSubstitutions:  Map[String, String]): Unit =
    writeCustomStylesheets()

    for document: Document <- documents; if inputFile(document).exists do
      context.lifecycle(s"DocBook: processing '${document.name}'.")

      val substitutions: Map[String, String] =
        (if document.substitutions.nonEmpty then document.substitutions else substitutionsDefault) ++
        globalSubstitutions

      // Write substitutions DTD (if there are any)
      if substitutions.nonEmpty then Files.write(
        file = layout.dtd(document.name),
        content = Catalog.dtd(substitutions)
      )

      // Generate data (if configured)
      if document.dataGeneratorClass.isDefined then
        val result: File = layout.data(document.name)
        result.mkdirs()
        context.javaexec(document.dataGeneratorClass.get, result.toString)

      for variantProcessor: VariantProcessor <- variantProcessors(document) do variantProcessor.process(
        context = context,
        layout = layout,
        substitutions = substitutions
      )

  private def writeCustomStylesheets(): Unit =
    def writeCustomStylesheet(name: String, section: Section): Unit =
      if !layout.customStylesheetSrc(name).exists then
        val body: ScalaXml.Nodes = section.customStylesheetBody
        if body.nonEmpty then Xsl.prettyPrinter.write(
          file = layout.customStylesheetTmp(name),
          element = Xsl.stylesheet(
            usesDocBookXslt2 = false,
            content = Seq(<!-- Customizations go here. -->) ++ body
          )
        )

    for xsltFormat: XsltFormat <-documents
      .flatMap(_.output)
      .map(_.format)
      .filter(_.isInstanceOf[XsltFormat])
      .map(_.asInstanceOf[XsltFormat])
    do
      writeCustomStylesheet(xsltFormat.name, xsltFormat)
      for common: Common <- xsltFormat.common do writeCustomStylesheet(common.fullName, common)

object DocBookProcessor:

  def getResolver(
    layout: Layout,
    context: BuildContext,
    documentName: String,
    into: File,
    xsltConfiguration: Option[(Xslt, File)]
  ): Resolver =
    val catalog: File = Files.file(into, Layout.catalogDirectory, "catalog.xml")
    Catalog.prettyPrinter.write(
      file = catalog,
      doctype = Some(Catalog),
      element = Catalog.catalog(catalogContent(
        xsltConfiguration,
        layout.customCatalog,
        layout.dtd(documentName),
        layout.data(documentName)
      ))
    )
    Resolver(catalog, context.getLogger)

  // TODO unfold
  private def catalogContent(
    xsltConfiguration: Option[(Xslt, File)],
    customCatalog: File,
    dtdFile: File,
    dataDirectory: File
  ): ScalaXml.Nodes =

    Seq(
      VariantProcessor.doNotEdit,
      ScalaXml.mkComment(s"customizations go into $customCatalog.")
    ) ++
    (if customCatalog.exists
     then Seq(Catalog.nextCatalog(customCatalog.getPath))
     else Seq(
       <!-- ... and had it existed, it'd be the next catalog here instead of the system one: -->,
       Catalog.nextCatalogSystem
     )
    ) ++
    (if !dtdFile.exists then Seq.empty else Seq(
      <!-- substitutions DTD -->,
      Catalog.public(publicId = DocBook.dtdId, uri = Files.file2url(dtdFile).toString)
    )) ++
    (if !dataDirectory.exists then Seq.empty else
      val rewritePrefix: String = Files.file2url(dataDirectory).toString
      Seq(<!-- generated data -->) ++
      (for dataSystemId <- Seq(
        "data:",
        "data:/",
        "urn:docbook:data:/",
        "urn:docbook:data:",
        "urn:docbook:data/",
        "http://opentorah.org/docbook/data/"
      ) yield Catalog.rewriteSystem(
        rewritePrefix = rewritePrefix,
        systemIdStartString = dataSystemId
      ))
    ) ++ xsltConfiguration.fold(Seq.empty)((xslt: Xslt, xsltDirectory: File) => Seq(
      ScalaXml.mkComment(s"DocBook ${xslt.name} stylesheets"),
      Catalog.rewriteUri(rewritePrefix = s"$xsltDirectory/", uriStartString = s"${xslt.uri}/")
    ))
