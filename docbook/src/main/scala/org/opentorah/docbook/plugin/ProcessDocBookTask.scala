package org.opentorah.docbook.plugin

import java.io.File
import org.gradle.api.logging.Logger
import org.gradle.api.DefaultTask
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, Internal, TaskAction}
import org.opentorah.docbook.{Layout, Operations, ProcessDocBookDirect, Stylesheets}
import org.opentorah.docbook.section.{DocBook2, Section, Sections, Variant}
import org.opentorah.fop.{FopFonts, MathJaxRunner}
import org.opentorah.mathjax.{Delimiters, MathJax, MathJaxConfiguration}
import org.opentorah.util.Collections.mapValues
import org.opentorah.util.{Files, Gradle}
import org.opentorah.xml.Resolver
import scala.beans.BeanProperty
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

  @Input @BeanProperty val document: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val documents: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val parameters: MapProperty[String, java.util.Map[String, String]] =
    getProject.getObjects.mapProperty(classOf[String], classOf[java.util.Map[String, String]])

  @Input @BeanProperty val substitutions: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @Input @BeanProperty val outputFormats: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val isMathJaxEnabled: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @Input @BeanProperty val useMathJax3: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @Input @BeanProperty val useJ2V8: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @Input @BeanProperty val mathJaxFont: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val mathJaxExtensions: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val texDelimiter: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val texInlineDelimiter: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val asciiMathDelimiter: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val isJEuclidEnabled: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @Input @BeanProperty val siteFile: Property[File] =
    getProject.getObjects.property(classOf[File])

  @Input @BeanProperty val xslt1version: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val xslt2version: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val nodeVersion: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val cssFile: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val epubEmbeddedFonts: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val dataGeneratorClass: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val processMathJaxEscapes: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

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
    require(!isMathJaxEnabled.get || !isJEuclidEnabled.get)

    val mathJaxConfiguration: MathJaxConfiguration = MathJaxConfiguration(
      font = mathJaxFont.get,
      extensions = mathJaxExtensions.get.asScala.toList,
      texDelimiters = Delimiters(texDelimiter.get),
      texInlineDelimiters = Delimiters(texInlineDelimiter.get),
      asciiMathDelimiters = Delimiters(asciiMathDelimiter.get),
      processEscapes = processMathJaxEscapes.get
    )

    val enableMathJax: Boolean = isMathJaxEnabled.get || isJEuclidEnabled.get

    val mathJaxRunner: Option[MathJaxRunner] =
      if (!processors.exists(_.isPdf) || !isMathJaxEnabled.get) None else Some(GradleOperations.getMathJaxRunner(
        getProject,
        nodeRoot = layout.nodeRoot,
        nodeVersion = nodeVersion.get,
        overwriteNode = false,
        overwriteMathJax = false,
        j2v8Parent = if (!useJ2V8.get) None else Some(layout.j2v8LibraryDirectory),
        mathJax = MathJax.get(useMathJax3 = useMathJax3.get),
        configuration = mathJaxConfiguration
      ))

    val substitutionsMap: Map[String, String] = substitutions.get.asScala.toMap

    // FOP configuration
    Operations.writeFopConfigurationFile(layout)

    // Catalog
    val resolver: Resolver = Operations.writeCatalog(
      layout,
      Some(xslt1),
      Some(xslt2),
      substitutionsMap
    )

    // CSS file
    val cssFileName: String = Files.dropAllowedExtension(cssFile.get, "css")
    Operations.writeCssFile(layout, cssFileName)

    // Input documents
    for ((name: String, _ /*prefixed*/: Boolean) <- inputDocuments) Operations.writeInputFile(layout, name)

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
      useMathJax3 = useMathJax3.get,
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
        inputFile = layout.inputFile(documentName),
        // In processing instructions and CSS, substitute xslParameters also - because why not?
        substitutions = allSubstitutions,
        mathJaxRunner = mathJaxRunner,
        resolver = resolver,
        stylesheetFile = layout.stylesheetFile(forDocument.mainStylesheet(variant)),
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
          isJEuclidEnabled = isJEuclidEnabled.get,
          mathJaxRunner = mathJaxRunner
        )
      }
    }

    if (siteFile.get != Extension.dummySiteFile) ProcessDocBookDirect.run(
      layout = layout,
      siteFile = siteFile.get,
      inputDocuments = inputDocuments,
      resolver = resolver
    )
  }
}
