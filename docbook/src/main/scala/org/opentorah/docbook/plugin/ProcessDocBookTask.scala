package org.opentorah.docbook.plugin

import java.io.File
import org.gradle.api.logging.Logger
import org.gradle.api.DefaultTask
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, Internal, TaskAction}
import org.opentorah.docbook.{Layout, Operations, Stylesheets}
import org.opentorah.docbook.section.{DocBook2, Section, Sections, Variant}
import org.opentorah.fop.{FopFonts, MathJaxRunner}
import org.opentorah.mathjax.{Delimiters, MathJax, MathJaxConfiguration}
import org.opentorah.util.Collections.mapValues
import org.opentorah.util.{Files, Gradle}
import org.opentorah.xml.Resolver
import scala.jdk.CollectionConverters._

// Note: Task class can not be final for Gradle to be able to decorate it.
class ProcessDocBookTask extends DefaultTask {
  setDescription(s"Process DocBook")
  setGroup("publishing")

  private val layout: Layout = DocBookPlugin.layoutForProject(getProject)
  private val logger: Logger = getProject.getLogger
  private def info(message: String): Unit = logger.info(message, null, null, null)

  // To let projects that use the plugin to not make assumptions about directory names:
  @Internal def getOutputDirectory: File = layout.outputRoot

  private def getProcessInputDirectories: Set[File] = Set(
    layout.inputDirectory,
    layout.cssDirectory,
    layout.fopConfigurationDirectory,
    layout.stylesheetDirectory
  ) ++ Set(
    layout.imagesDirectory
  ).filter(_.exists)

  // Register inputs

  getProcessInputDirectories.foreach(registerInputDirectory)

  // Data generation class doesn't have to reside in the same project where DocBook plugin is configured,
  // so I don't bother registering classes as input directory, but if I did:
  // classesDirs use sourceSets, which are only available after project is evaluated;
  // with code in Scala only, input directory "build/classes/java/main" doesn't exist (which is a Gradle error), so I
  // register "build/classes/" (grandparent of all classesDirs) as input directory
  // (is there a simpler way of getting it?).
  //  getProject.afterEvaluate((project: Project) =>
  //    Gradle.classesDirs(project).map(_.getParentFile.getParentFile).foreach(registerInputDirectory))

  private def registerInputDirectory(directory: File): Unit = {
    info(s"processDocBook: registering input directory $directory")
    directory.mkdirs()
    getInputs.dir(directory)
  }

  // Register outputs
  private def getProcessOutputDirectories: Set[File] = Set(
    layout.intermediateRoot,
    layout.outputRoot
  )
  for (directory: File <- getProcessOutputDirectories) {
    // Note: deleting directories makes the task not up-to-date: Files.deleteRecursively(directory)
    info(s"processDocBook: registering output directory $directory")
    getOutputs.dir(directory)
  }

  private val document: Property[String] = getProject.getObjects.property(classOf[String])
  @Input final def getDocument(): Property[String] = document

  private val documents: ListProperty[String] = getProject.getObjects.listProperty(classOf[String])
  @Input final def getDocuments(): ListProperty[String] = documents

  private val parameters: MapProperty[String, java.util.Map[String, String]] =
    getProject.getObjects.mapProperty(classOf[String], classOf[java.util.Map[String, String]])
  @Input final def getParameters(): MapProperty[String, java.util.Map[String, String]] = parameters

  private val substitutions: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])
  @Input final def getSubstitutions(): MapProperty[String, String] = substitutions

  private val outputFormats: ListProperty[String] = getProject.getObjects.listProperty(classOf[String])
  @Input final def getOutputFormats(): ListProperty[String] = outputFormats

  private val mathJaxEnabled: Property[Boolean] = getProject.getObjects.property(classOf[Boolean])
  @Input final def getMathJaxEnabled(): Property[Boolean] = mathJaxEnabled

  private val useMathJax3: Property[Boolean] = getProject.getObjects.property(classOf[Boolean])
  @Input final def getUseMathJax3(): Property[Boolean] = useMathJax3

  private val useJ2V8: Property[Boolean] = getProject.getObjects.property(classOf[Boolean])
  @Input final def getUseJ2V8(): Property[Boolean] = useJ2V8

  private val mathJaxFont: Property[String] = getProject.getObjects.property(classOf[String])
  @Input final def getMathJaxFont(): Property[String] = mathJaxFont

  private val mathJaxExtensions: ListProperty[String] = getProject.getObjects.listProperty(classOf[String])
  @Input final def getMathJaxExtensions(): ListProperty[String] = mathJaxExtensions

  private val texDelimiter: Property[String] = getProject.getObjects.property(classOf[String])
  @Input final def getTexDelimiter(): Property[String] = texDelimiter

  private val texInlineDelimiter: Property[String] = getProject.getObjects.property(classOf[String])
  @Input final def getTexInlineDelimiter(): Property[String] = texInlineDelimiter

  private val asciiMathDelimiter: Property[String] = getProject.getObjects.property(classOf[String])
  @Input final def getAsciiMathDelimiter(): Property[String] = asciiMathDelimiter

  private val jEuclidEnabled: Property[Boolean] = getProject.getObjects.property(classOf[Boolean])
  @Input final def getJEuclidEnabled(): Property[Boolean] = jEuclidEnabled

  private val xslt1version: Property[String] = getProject.getObjects.property(classOf[String])
  @Input final def getXslt1version(): Property[String] = xslt1version

  private val xslt2version: Property[String] = getProject.getObjects.property(classOf[String])
  @Input final def getXslt2version(): Property[String] = xslt2version

  private val nodeVersion: Property[String] = getProject.getObjects.property(classOf[String])
  @Input final def getNodeVersion(): Property[String] = nodeVersion

  private val cssFile: Property[String] = getProject.getObjects.property(classOf[String])
  @Input final def getCssFile(): Property[String] = cssFile

  private val epubEmbeddedFonts: ListProperty[String] = getProject.getObjects.listProperty(classOf[String])
  @Input final def getEpubEmbeddedFonts(): ListProperty[String] = epubEmbeddedFonts

  private val dataGeneratorClass: Property[String] = getProject.getObjects.property(classOf[String])
  @Input final def getDataGeneratorClass(): Property[String] = dataGeneratorClass

  private val processMathJaxEscapes: Property[Boolean] = getProject.getObjects.property(classOf[Boolean])
  @Input final def getProcessMathJaxEscapes(): Property[Boolean] = processMathJaxEscapes

  @TaskAction
  def processDocBook(): Unit = {
    def getDocumentName(string: String): Option[String] =
      if (string.isEmpty) None else Some(Files.dropAllowedExtension(string, "xml"))

    val documentName: Option[String] = getDocumentName(document.get)
    val documentNames: List[String] = documents.get.asScala.toList.flatMap(getDocumentName)

    if (documentName.isEmpty && documentNames.isEmpty) throw new IllegalArgumentException(
      """At least one document name must be specified using
        |  document = "<document name>"
        |or
        |  documents = ["<document name>"]
        |""".stripMargin)

    val inputDocuments: List[(String, Boolean)] =
      documentName.toList.map(name => name -> false) ++
      documentNames.map(name => name -> true)

    val processors: List[DocBook2] = Option(getProject.findProperty("docBook.outputFormats"))
      .map(_.toString.split(",").map(_.trim).toList.filter(_.nonEmpty))
      .getOrElse(outputFormats.get.asScala.toList)
      .map(DocBook2.forName)

    info(s"Output formats: ${DocBook2.getNames(processors)}")

    val sections: Sections = Sections(mapValues(parameters.get.asScala.toMap)(_.asScala.toMap))

    val unusedSections: Set[Section] =
      sections.usedSections -- processors.flatMap(_.parameterSections).toSet

    if (unusedSections.nonEmpty)
      info(s"Unused parameter sections: ${unusedSections.map(_.name).mkString(", ")}")

    // Generate data
    val mainClass: String = dataGeneratorClass.get
    if (mainClass.isEmpty) info("Skipping DocBook data generation: 'dataGenerationClass' is not set")
    else GradleOperations.generateData(getProject, mainClass, layout.dataDirectory)

    // Unpack DocBook stylesheets
    val xslt1: File = GradleOperations.unpackStylesheets(getProject, Stylesheets.xslt1, xslt1version.get, layout.docBookXslDirectory)
    val xslt2: File = GradleOperations.unpackStylesheets(getProject, Stylesheets.xslt2, xslt2version.get, layout.docBookXslDirectory)

    // MathJax
    require(!getMathJaxEnabled().get || !getJEuclidEnabled().get)

    val mathJaxConfiguration: MathJaxConfiguration = MathJaxConfiguration(
      font = mathJaxFont.get,
      extensions = mathJaxExtensions.get.asScala.toList,
      texDelimiters = Delimiters(texDelimiter.get),
      texInlineDelimiters = Delimiters(texInlineDelimiter.get),
      asciiMathDelimiters = Delimiters(asciiMathDelimiter.get),
      processEscapes = processMathJaxEscapes.get
    )

    val enableMathJax: Boolean = getMathJaxEnabled().get || getJEuclidEnabled().get
    val mathJax: MathJax = MathJax.get(useMathJax3 = getUseMathJax3().get)

    val mathJaxRunner: Option[MathJaxRunner] =
      if (!processors.exists(_.isPdf) || !getMathJaxEnabled().get) None else Some(GradleOperations.getMathJaxRunner(
        getProject,
        nodeRoot = layout.nodeRoot,
        nodeVersion = nodeVersion.get,
        overwriteNode = false,
        overwriteMathJax = false,
        j2v8Parent = if (!useJ2V8.get) None else Some(layout.j2v8LibraryDirectory),
        mathJax = mathJax,
        configuration = mathJaxConfiguration
      ))

    val substitutionsMap: Map[String, String] = substitutions.get.asScala.toMap

    // FOP configuration
    Operations.writeFopConfigurationFile(layout)

    // Catalog
    Operations.writeCatalog(
      layout,
      xslt1,
      xslt2,
      substitutionsMap
    )

    val resolver: Resolver = new Resolver(layout.catalogFile)

    // CSS file
    val cssFileName: String = Files.dropAllowedExtension(cssFile.get, "css")
    Operations.writeCssFile(layout, cssFileName)

    // Input documents
    Operations.writeInputDocuments(layout, inputDocuments)

    // Fonts
    val fontFamilyNames: List[String] = epubEmbeddedFonts.get.asScala.toList
    val epubEmbeddedFontsString: String = FopFonts.getFiles(layout.fopConfigurationFile, fontFamilyNames)
      .map(uri => new File(uri.getPath).getAbsolutePath).mkString(", ")
    info(s"Fop.getFontFiles(${fontFamilyNames.mkString(", ")}) = $epubEmbeddedFontsString.")

    Operations.writeStylesheets(
      layout = layout,
      sections = sections,
      inputDocuments = inputDocuments,
      isInfoEnabled = logger.isInfoEnabled,
      embeddedFonts = epubEmbeddedFontsString,
      cssFileName = cssFileName,
      mathJax = mathJax,
      mathJaxConfiguration = mathJaxConfiguration,
      enableMathJax = enableMathJax
    )

    val variants: Seq[Variant] = sections.runVariants(processors)
    logger.lifecycle("Output variants: " + variants.map(_.fullName).mkString("[", ", ", "]"))

    val allSubstitutions: Map[String, String] = sections.substitutions ++ substitutionsMap

    // Process DocBook :)
    for {
      variant: Variant <- variants
      (documentName: String, prefixed: Boolean) <- inputDocuments
    } {
      logger.lifecycle(s"DocBook: processing '$documentName' to ${variant.fullName}.")
      val docBook2: DocBook2 = variant.docBook2
      val forDocument: Layout.ForDocument = layout.forDocument(prefixed, documentName)
      val saxonOutputDirectory: File = forDocument.saxonOutputDirectory(variant)
      val saxonOutputFile: File = forDocument.saxonOutputFile(variant)

      saxonOutputDirectory.mkdirs

      Operations.runSaxon(
        docBook2 = docBook2,
        inputFile = forDocument.inputFile,
        // In processing instructions and CSS, substitute xslParameters also - because why not?
        substitutions = allSubstitutions,
        mathJaxRunner = mathJaxRunner,
        resolver = resolver,
        stylesheetFile = forDocument.mainStylesheetFile(variant),
        saxonOutputFile = saxonOutputFile
      )

      val into: File = Files.prefixedDirectory(saxonOutputDirectory, docBook2.copyDestinationDirectoryName)

      info(s"Copying images")
      Gradle.copyDirectory(
        getProject,
        into,
        from = layout.imagesDirectory.getParentFile,
        directoryName = layout.imagesDirectoryName,
        substitutions = Map.empty
      )

      if (docBook2.usesCss) {
        info(s"Copying CSS")
        Gradle.copyDirectory(
          getProject,
          into,
          from = layout.cssDirectory.getParentFile,
          directoryName = layout.cssDirectoryName,
          substitutions = allSubstitutions
        )
      }

      if (docBook2.usesIntermediate) {
        info(s"Post-processing ${docBook2.name}")
        forDocument.outputDirectory(variant).mkdirs

        Operations.postProcess(
          layout = layout,
          docBook2 = docBook2,
          outputFile = forDocument.outputFile(variant),
          saxonOutputDirectory = saxonOutputDirectory,
          saxonOutputFile = saxonOutputFile,
          substitutions = allSubstitutions,
          isJEuclidEnabled = getJEuclidEnabled().get,
          mathJaxRunner = mathJaxRunner
        )
      }
    }
  }
}
