package org.podval.docbook.gradle.plugin

import java.io.File

import org.gradle.api.{DefaultTask, Task}
import org.gradle.api.plugins.JavaPluginConvention
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, Internal, SourceSet, TaskAction}
import org.gradle.process.JavaExecSpec
import org.podval.docbook.gradle.fop.Fop
import org.podval.docbook.gradle.mathjax
import org.podval.docbook.gradle.section.{DocBook2, Section}
import org.podval.docbook.gradle.util.{Gradle, Logger, Util}
import org.podval.docbook.gradle.xml.Resolver

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

  @Input @BeanProperty val useJ2V8: Property[Boolean] =
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

    val substitutionsMap: Map[String, String] = substitutions.get.asScala.toMap

    write.writeFopConfiguration()
    write.substitutionsDtd(substitutionsMap)
    write.xmlCatalog()
    write.xmlCatalogCustomization()

    val cssFileName: String = Util.dropAllowedExtension(cssFile.get, "css")

    write.css(cssFileName)

    for ((name: String, _ /*prefixed*/: Boolean) <- inputDocuments)
      write.inputFile(name)

    for {
      docBook2: DocBook2 <- DocBook2.all
      (documentName: String, prefixed: Boolean) <- inputDocuments
    } write.mainStylesheet(
      docBook2,
      prefixed,
      documentName,
      cssFileName,
      epubEmbeddedFonts = Fop.getFontFiles(layout.fopConfigurationFile, epubEmbeddedFonts.get.asScala.toList, logger)
    )

    for (docBook2: DocBook2 <- DocBook2.all)
      write.paramsStylesheet(docBook2, sections)

    for (section: Section <- Section.all)
      write.customStylesheet(section)

    generateData()

    require(!isMathJaxEnabled.get || !isJEuclidEnabled.get)

    // TODO make MathJax configurable via extension
    val mathJaxConfiguration: mathjax.Configuration = mathjax.Configuration()

    val processDocBook: ProcessDocBook = new ProcessDocBook(
      getProject,
      // In processing instructions and CSS, substitute xslParameters also - because why not?
      substitutions = sections.values.toList.flatten.toMap ++ substitutionsMap,
      resolver = new Resolver(layout.catalogFile, logger),
      isJEuclidEnabled.get,
      mathJaxTypesetter =
        if (!isMathJaxEnabled.get) None
        else Some(new MathJax(getProject, useJ2V8.get, layout, logger).getTypesetter(mathJaxConfiguration)),
      layout,
      logger
    )

    for {
      docBook2: DocBook2 <- processors
      (documentName: String, prefixed: Boolean) <- inputDocuments
    } processDocBook.run(
      docBook2,
      prefixed,
      documentName
    )
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
}
