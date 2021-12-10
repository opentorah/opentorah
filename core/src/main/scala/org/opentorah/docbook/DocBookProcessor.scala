package org.opentorah.docbook

import org.opentorah.fop.FopFonts
import org.opentorah.html.SiteHtml
import org.opentorah.math.MathConfiguration
import org.opentorah.util.{BuildContext, Distribution, Files}
import org.opentorah.xml.{Catalog, ScalaXml, Xsl}
import java.io.File

// TODO combine epubEmbeddedFonts, xslt1version and xslt2version?
// TODO extract PDF author/title from the document itself
// TODO if I really need additional attributes - generalize Xsl.stylesheet() to take them:
//    xmlns:db={DocBook.namespace.uri}
//    exclude-result-prefixes="db"
final class DocBookProcessor(
  layout: Layout,
  context: BuildContext,
  siteHtml: SiteHtml,
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

  def inputFiles: Set[File] = for
    document: Document <- documents
    file = inputFile(document)
    if file.exists
  yield file

  def verify(): Unit =
    def logUnused[T <: HasName](whats: String, unused: Set[T]): Unit = if unused.nonEmpty then
      val unusedString: String = unused.map(_.name).mkString(", ")
      context.warn(s"DocBook: unused $whats: $unusedString")

    for
      variant <- variants
      if variant.format.isInstanceOf[DirectFormat]
      if variant.parameters.nonEmpty
    do context.warn(s"DocBook: direct format variant has parameters configured: ${variant.name}")

    val variantsUsed: Set[Variant] = documents.flatMap(_.output)
    logUnused("variants", variants -- variantsUsed)

    val formats: Set[Format] = variants.filter(_.parameters.nonEmpty).map(_.format)
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

  def installDistibutions: Unit =
    for distribution: Distribution[_] <- distributionsNeeded do
      context.info(s"DocBook: checking dependency: $distribution")
      val installationOpt: Option[_] = distribution.getInstallation(context)
      if installationOpt.isEmpty then context.warn(s"DocBook: can not install needed dependency: $distribution")

  def distributionsNeeded: Set[Distribution[_]] = (
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
        siteHtml.math,
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

      val xslt: Option[Xslt] = format match
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

      val documentTmp: File = Files.file(layout.tmp, document.name)

      val substitutions: Map[String, String] =
        (if document.substitutions.nonEmpty then document.substitutions else substitutionsDefault) ++
        globalSubstitutions

      // Write substitutions DTD (if there are any)
      val dtdFile: Option[File] = if substitutions.isEmpty then None else
        val result: File = Files.file(documentTmp, "substitutions.dtd")
        Files.write(
          file = result,
          content = Catalog.dtd(substitutions)
        )
        Some(result)

      // Generate data (if configured)
      val dataDirectory: Option[File] = if document.dataGeneratorClass.isEmpty then None else
        val result: File = Files.file(documentTmp, "data")
        result.mkdirs()
        context.javaexec(document.dataGeneratorClass.get, result.toString)
        Some(result)

      for variantProcessor: VariantProcessor <- variantProcessors(document) do variantProcessor.process(
        context = context,
        layout = layout,
        siteHtml = siteHtml,
        substitutions = substitutions,
        documentTmp = documentTmp,
        dtdFile = dtdFile,
        dataDirectory = dataDirectory
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
