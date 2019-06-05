package org.podval.docbook.gradle.plugin

import java.io.File

import org.gradle.api.{DefaultTask, Task}
import org.gradle.api.plugins.JavaPluginConvention
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, Internal, SourceSet, TaskAction}
import org.gradle.process.JavaExecSpec
import org.podval.docbook.gradle.fop.Fop
import org.podval.docbook.gradle.mathjax.MathJax
import org.podval.docbook.gradle.node.{Distribution, Installation}
import org.podval.docbook.gradle.section.{DocBook2, Section}
import org.podval.docbook.gradle.util.{Architecture, Gradle, Logger, Os, Platform, Util}
import org.podval.docbook.gradle.xml.{ProcessingInstructionsFilter, Resolver, Xml}

import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._

class ProcessDocBookTask extends DefaultTask {

  private val layout: Layout = Layout.forProject(getProject)
  private val logger: Logger = Logger.forProject(getProject)
  private def info(message: String): Unit = logger.info(message)

  private val write: Write = new Write(layout, logger)

  // To let projects that use the plugin to not make assumptions about directory names:
  @Internal def getOutputDirectory: File = layout.outputRoot

  private def getProcessInputDirectories: Set[File] = Set(
    layout.inputDirectory,
    layout.cssDirectory,
    layout.fopConfigurationDirectory,
    layout.stylesheetDirectory
  ) ++ Set(
    layout.imagesDirectory,
  ).filter(_.exists)

  // Register inputs
  for (directory: File <- getProcessInputDirectories) {
    info(s"Registering input directory $directory")
    directory.mkdirs()
    getInputs.dir(directory)
  }

  // Register outputs
  private def getProcessOutputDirectories: Set[File] = Set(
    layout.intermediateRoot,
    layout.outputRoot
  )
  for (directory: File <- getProcessOutputDirectories) {
    Util.deleteRecursively(directory)
    info(s"Registering output directory $directory")
    getOutputs.dir(directory)
  }

  @Input @BeanProperty val document: Property[String] =
    getProject.getObjects.property(classOf[String])

  @BeanProperty val documents: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @BeanProperty val parameters: MapProperty[String, java.util.Map[String, String]] =
    getProject.getObjects.mapProperty(classOf[String], classOf[java.util.Map[String, String]])

  @Input @BeanProperty val substitutions: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @Input @BeanProperty val outputFormats: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val isMathJaxEnabled: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @Input @BeanProperty val isJEuclidEnabled: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @BeanProperty val xslt1version: Property[String] =
    getProject.getObjects.property(classOf[String])

  @BeanProperty val xslt2version: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val cssFile: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val epubEmbeddedFonts: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val dataGeneratorClass: Property[String] =
    getProject.getObjects.property(classOf[String])

  @TaskAction
  def processDocBook(): Unit = {
    val documentName: Option[String] = getDocumentName(document.get)
    val documentNames: List[String] = documents.get.asScala.toList.flatMap(getDocumentName)

    if (documentName.isEmpty && documentNames.isEmpty)
      throw new IllegalArgumentException(
        """At least one document name must be specified using
          |  document = "<document name>"
          |or
          |  documents = ["<document name>"]
          |""".stripMargin)

    val inputDocuments: List[(String, Boolean)] =
      documentName.toList.map(name => name -> false) ++
      documentNames.map(name => name -> true)

    val processors: List[DocBook2] =
      Option(getProject.findProperty("docBook.outputFormats"))
        .map(_.toString.split(",").map(_.trim).toList.filter(_.nonEmpty))
        .getOrElse(outputFormats.get.asScala.toList)
        .map(DocBook2.forName)

    info(s"Output formats: ${DocBook2.getNames(processors)}")

    val sections: Map[Section, Map[String, String]] =
      for ((sectionName: String, sectionParameters: Map[String, String]) <-
           parameters.get.asScala.toMap.view.mapValues(_.asScala.toMap).toMap)
        yield (Section.forName(sectionName), sectionParameters)

    val unusedSections: Set[Section] =
      sections.keySet -- processors.flatMap(_.parameterSections).toSet

    if (unusedSections.nonEmpty)
      info(s"Unused parameter sections: ${unusedSections.map(_.name).mkString(", ")}")

    Stylesheets.xslt1.unpack(xslt1version.get, getProject, layout, logger)
    Stylesheets.xslt2.unpack(xslt2version.get, getProject, layout, logger)

    // Make sure MathJax is installed
    // TODO only do this if isMathJaxEnabled
    // TODO try J2V8 and fall back to this
    // TODO feed resulting MathJax into the process
    installMathJax

    // TODO make configurable via extension
    val mathJaxConfiguration: MathJax.Configuration = MathJax.Configuration()

    val substitutionsMap: Map[String, String] = substitutions.get.asScala.toMap

    write.writeFopConfiguration()
    write.substitutionsDtd(substitutionsMap)
    write.xmlCatalog()
    write.xmlCatalogCustomization()

    val cssFileName: String = Util.dropAllowedExtension(cssFile.get, "css")

    write.css(cssFileName)

    for ((name: String, _ /*prefixed*/: Boolean) <- inputDocuments)
      write.inputFile(name)

    val epubEmbeddedFontsStr: String = {
      val fontNames: List[String] = epubEmbeddedFonts.get.asScala.toList
      if (fontNames.isEmpty) "" else Fop.getFontFiles(layout.fopConfigurationFile, fontNames, logger)
    }

    for {
      docBook2: DocBook2 <- DocBook2.all
      (documentName: String, prefixed: Boolean) <- inputDocuments
    } write.mainStylesheet(
      docBook2,
      prefixed,
      documentName,
      cssFileName,
      epubEmbeddedFontsStr
    )

    for (docBook2: DocBook2 <- DocBook2.all)
      write.paramsStylesheet(docBook2, sections)

    for (section: Section <- Section.all)
      write.customStylesheet(section)

    generateData()

    // In processing instructions and CSS, substitute xslParameters also - because why not?
    val allSubstitutions: Map[String, String] = sections.values.toList.flatten.toMap ++ substitutionsMap

    val resolver: Resolver = new Resolver(layout.catalogFile, logger)

    for {
      docBook2: DocBook2 <- processors
      (documentName: String, prefixed: Boolean) <- inputDocuments
    } run(
      docBook2,
      prefixed,
      documentName,
      allSubstitutions,
      resolver,
      isJEuclidEnabled.get,
      isMathJaxEnabled.get,
      mathJaxConfiguration
    )
  }

  private def installMathJax: Installation = {
    val version: String = Distribution.defaultVersion
    val os: Os = Platform.getOs
    val arch: Architecture = Platform.getArch

    val distribution: Distribution = new Distribution(version, os, arch)
    val installation: Installation = new Installation(distribution, layout.nodeRoot, layout.nodeModulesRoot)

    if (!installation.root.exists()) {
      info(s"Installing Node v$version for $os on $arch")
      installation.install(getProject)
    }

    if (!installation.nodeModules.exists()) {
      info(s"Installing mathjax-node")
      installation.npmInstall("mathjax-node")
    }

    installation
  }

  private def getDocumentName(string: String): Option[String] =
    if (string.isEmpty) None else Some(Util.dropAllowedExtension(string, "xml"))

  // TODO handle order of application and remove dependency instructions from the README.md
  private def generateData(): Unit = {
    val mainClass: String = dataGeneratorClass.get
    val mainSourceSet: Option[SourceSet] =
      Option(getProject.getConvention.findPlugin(classOf[JavaPluginConvention]))
        .map(_.getSourceSets.getByName(SourceSet.MAIN_SOURCE_SET_NAME))
    val classesTask: Option[Task] = Gradle.getClassesTask(getProject)
    val dataDirectory: File = layout.dataDirectory

    if (mainClass.isEmpty) info("Skipping DocBook data generation: dataGenerationClass is not set") else
    if (mainSourceSet.isEmpty) info("Skipping DocBook data generation: no Java plugin in the project") else
    if (classesTask.isEmpty) info("Skipping DocBook data generation: no 'classes' task in the project") else
    if (!classesTask.get.getDidWork) info("Skipping DocBook data generation: 'classes' task didn't do work") else
//    if (dataDirectory.exists) info(s"Skipping DocBook data generation: directory $dataDirectory exists") else
    {
      info(s"Running DocBook data generator $mainClass into $dataDirectory")
      getProject.javaexec((exec: JavaExecSpec) => {
        exec.setClasspath(mainSourceSet.get.getRuntimeClasspath)
        exec.setMain(mainClass)
        exec.args(dataDirectory.toString)
       })
    }
  }

  def run(
    docBook2: DocBook2,
    prefixed: Boolean,
    documentName: String,
    substitutions: Map[String, String],
    resolver: Resolver,
    isJEuclidEnabled: Boolean,
    isMathJaxEnabled: Boolean,
    mathJaxConfiguration: MathJax.Configuration
  ): Unit = {
    logger.lifecycle(s"DocBook: processing '$documentName' to ${docBook2.name}.")

    val forDocument: Layout.ForDocument = layout.forDocument(prefixed, documentName)

    // Saxon output directory.
    val saxonOutputDirectory: File = forDocument.saxonOutputDirectory(docBook2)
    saxonOutputDirectory.mkdirs

    // Saxon output file and target.
    val saxonOutputFile: File = forDocument.saxonOutputFile(docBook2)

    // Run Saxon.
    Xml.transform(
      useSaxon9 = docBook2.usesDocBookXslt2,
      resolver = resolver,
      inputFile = layout.inputFile(documentName),
      stylesheetFile = layout.stylesheetFile(forDocument.mainStylesheet(docBook2)),
      // do not output the 'main' file when chunking in XSLT 1.0
      outputFile = if (docBook2.usesRootFile) Some(saxonOutputFile) else None,
      xmlReader = Xml.getFilteredXMLReader(
        Seq(new ProcessingInstructionsFilter(substitutions, resolver, logger)) ++
          docBook2.xmlFilter(mathJaxConfiguration).toSeq // ++ Seq(new TracingFilter)
      ),
      logger = logger
    )

    copyImagesAndCss(docBook2, saxonOutputDirectory, substitutions)

    // Post-processing.
    if (docBook2.usesIntermediate) {
      info(s"Post-processing ${docBook2.name}")
      val outputDirectory: File = forDocument.outputDirectory(docBook2)
      outputDirectory.mkdirs

      docBook2.postProcess(
        fopConfigurationFile = layout.fopConfigurationFile,
        nodeModulesRoot = layout.nodeModulesRoot,
        substitutions = substitutions,
        isMathJaxEnabled = isMathJaxEnabled,
        isJEuclidEnabled = isJEuclidEnabled,
        mathJaxConfiguration = mathJaxConfiguration,
        inputDirectory = saxonOutputDirectory,
        inputFile = saxonOutputFile,
        outputFile = forDocument.outputFile(docBook2),
        logger = logger
      )
    }
  }

  private def copyImagesAndCss(
    docBook2: DocBook2,
    saxonOutputDirectory: File,
    substitutions: Map[String, String]
  ): Unit = {
    val into: File = Util.prefixedDirectory(saxonOutputDirectory, docBook2.copyDestinationDirectoryName)

    info(s"Copying images")
    Gradle.copyDirectory(getProject,
      into = into,
      from = layout.imagesDirectory.getParentFile,
      directoryName = layout.imagesDirectoryName
    )

    if (docBook2.usesCss) {
      info(s"Copying CSS")
      Gradle.copyDirectory(getProject,
        into = into,
        from = layout.cssDirectory.getParentFile,
        directoryName = layout.cssDirectoryName,
        substitutions = substitutions
      )
    }
  }
}
