package org.opentorah.docbook

import org.opentorah.build.BuildContext
import org.opentorah.files.Copy
import org.opentorah.fop.{Fop, FopPlugin}
import org.opentorah.math.{DocBookMathFilter, MathConfiguration, MathJax, MathJaxFopPlugin, MathJaxRunner}
import org.opentorah.node.{Node, NodeInstallation}
import org.opentorah.util.Files
import org.opentorah.xml.{Resolver, Sax, Saxon, ScalaXml, Xsl}
import java.io.File

trait XsltFormat extends Format, Section:

  final def process(
    layout: Layout,
    context: BuildContext,
    documentName: String,
    substitutions: Map[String, String],
    resolver: Resolver,
    imagesDirectory: Option[String],
    inputFile: File,
    tmp: File,
    processOutputDirectory: File,
    processOutputFile: File,
    finalOutputFile: File,
    variantName: String,
    parameters: Parameters,
    mathConfiguration: MathConfiguration,
    epubEmbeddedFontsString: Option[String],
    xslt: Xslt,
    customStylesheets: Seq[File]
  ): Unit =
    // Layout
    def stylesheet(suffix: String): File = Files.file(tmp, Layout.xslDirectory, variantName + suffix + ".xsl")
    val mainStylesheet: File = stylesheet("")
    val parametersStylesheet: File = stylesheet("-params")

    val isPdf: Boolean = this == Pdf

    // Calculated parameters
    val calculatedParameters: CalculatedParameters = CalculatedParameters(
      layout,
      documentName,
      this,
      processOutputDirectory,
      mathConfiguration,
      epubEmbeddedFontsString,
      context.getLogger
    )

    // Main stylesheet
    Xsl.prettyPrinter.write(
      file = mainStylesheet,
      element = Xsl.stylesheet(
        usesDocBookXslt2,
        mainStylesheetBody(
          parametersStylesheet = parametersStylesheet,
          customStylesheets = customStylesheets,
          stylesheetUriBase = xslt.uri,
          values = calculatedParameters
        )
      )
    )

    // Parameters stylesheet
    Xsl.prettyPrinter.write(
      file = parametersStylesheet,
      element = Xsl.stylesheet(usesDocBookXslt2, VariantProcessor.doNotEdit +: parameters.bySectionXml)
    )

    // Saxon
    context.info(s"Running Saxon")
    processOutputDirectory.mkdirs

    (if xslt.usesSaxon6 then Saxon.Saxon6 else Saxon.Saxon11).transform(
      filters =
        Seq(EvalFilter(substitutions)) ++
        (if !(isPdf && mathConfiguration.mathJaxEnabled.contains(true)) then Seq.empty else Seq(DocBookMathFilter(mathConfiguration))),
        // ++ Seq(new TracingFilter),
      resolver = Some(resolver),
      stylesheetFile = Some(mainStylesheet),
      inputSource = Sax.file2inputSource(inputFile),
      result = Saxon.result(usesRootFile, processOutputFile),
      logger = context.getLogger
    )

    // Images and CSS
    val into: File = copyDestinationDirectoryName.fold(processOutputDirectory)(suffix => File(processOutputDirectory, suffix))

    val imagesDirectoryFile: File = File(layout.src, imagesDirectory.getOrElse(Layout.imagesDirectory))
    if imagesDirectoryFile.exists then
      context.info(s"Copying images")
      Copy.copyTo(
        from = imagesDirectoryFile,
        into = File(into, Layout.imagesDirectory),
        substitutions = Map.empty
      )

    if usesCss then
      context.info(s"Copying CSS")
      Copy.copyInto(
        from = File(layout.src, Layout.cssDirectory),
        into = into,
        substitutions = substitutions
      )

    // TODO move into Pdf.postProcess()
    // Post-process with FOP
    if isPdf then
      context.info(s"Running FOP")
      Fop.run(
        saxon = Saxon.Saxon11,
        configurationFile = layout.fopConfigurationFile,
        creationDate = substitutions.get("creationDate"),
        author = substitutions.get("author"),
        title = substitutions.get("title"),
        subject = substitutions.get("subject"),
        keywords = substitutions.get("keywords"),
        inputFile = processOutputFile,
        outputFile = finalOutputFile,
        plugin = fopPlugin(mathConfiguration, context),
        logger = context.getLogger
      )

  private def fopPlugin(
    mathConfiguration: MathConfiguration,
    context: BuildContext
  ): Option[FopPlugin] = if !mathConfiguration.enableMathJax then None else
      // Make sure MathJax is installed
      val nodeInstallation: NodeInstallation =
        mathConfiguration.nodeDistribution.getInstallation(context, installIfDoesNotExist = false, mustExist = true)
      val node: Node = nodeInstallation.getNode(nodeInstallation.getRoot)
      val npmPackagesToInstall: List[String] = MathJax(mathConfiguration).npmPackagesToInstall
      node.mkNodeModules()
      node.npm(
        arguments = "install " + npmPackagesToInstall.mkString(" "),
        log = (message: String) => ()
      )
      Some(MathJaxFopPlugin(MathJaxRunner(node, mathConfiguration)))

  // xsl:param has the last value assigned to it, so customization must come last;
  // since it is imported (so as not to be overwritten), and import elements must come first,
  // a separate "-param" file is written with the "default" values for the parameters :)
  private def mainStylesheetBody(
    parametersStylesheet: File,
    customStylesheets: Seq[File],
    stylesheetUriBase: String,
    values: CalculatedParameters
  ): ScalaXml.Nodes = Seq(
    VariantProcessor.doNotEdit,
    Xsl.xslImport(s"$stylesheetUriBase/$stylesheetUriName.xsl"),
    Xsl.xslImport(parametersStylesheet.toString),
    <!-- Custom stylesheets -->
  ) ++ (
    for customStylesheet <- customStylesheets yield
      if customStylesheet.exists
      then Xsl.xslImport(customStylesheet.toString)
      else ScalaXml.mkComment(s" If it existed, the following file would be imported: $customStylesheet ")
  ) ++ Seq(
    <!-- Calculated parameters -->
  ) ++ Parameters.parametersBySection(
    common.map((common: Common) => common.fullName -> common.calculatedParameters(values)) :+
    (name, calculatedParameters(values))
  ) ++ mainStylesheetCalculated(values)

  protected def mainStylesheetCalculated(values: CalculatedParameters): ScalaXml.Nodes

  final def xslVersion: String = Xsl.version(usesDocBookXslt2)

  def usesDocBookXslt2: Boolean = false

  protected def stylesheetUriName: String

  def usesRootFile: Boolean

  def usesCss: Boolean

  def copyDestinationDirectoryName: Option[String] = None
