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
import org.podval.docbook.gradle.util.{Gradle, Logger, Platform, Util}
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

  @Input @BeanProperty val xslt1version: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val xslt2version: Property[String] =
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

    require(!isMathJaxEnabled.get || !isJEuclidEnabled.get)

    val mathJaxConfiguration: mathjax.Configuration = getMathJaxConfiguration

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

    val mathJax: Option[mathjax.MathJax] =
      if (!processors.exists(_.isPdf) && !isMathJaxEnabled.get) None else Some(mathjax.MathJax.get(
        getProject,
        Platform.getOs,
        Platform.getArch,
        layout.nodeRoot,
        useJ2V8.get,
        layout.j2v8LibraryDirectory,
        mathJaxConfiguration,
        logger
      ))

    val processDocBook: ProcessDocBook = new ProcessDocBook(
      getProject,
      // In processing instructions and CSS, substitute xslParameters also - because why not?
      substitutions = sections.values.toList.flatten.toMap ++ substitutionsMap,
      resolver = new Resolver(layout.catalogFile, logger),
      isJEuclidEnabled.get,
      mathJax = mathJax,
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

  private def generateData(): Unit = {
    val mainClass: String = dataGeneratorClass.get
    val mainSourceSet: Option[SourceSet] =
      Option(getProject.getConvention.findPlugin(classOf[JavaPluginConvention]))
        .map(_.getSourceSets.getByName(SourceSet.MAIN_SOURCE_SET_NAME))
    val classesTask: Option[Task] = Gradle.getClassesTask(getProject)
    val dataDirectory: File = layout.dataDirectory

    def skipping(message: String): Unit = info(s"Skipping DocBook data generation: $message")
    if (mainClass.isEmpty) skipping("dataGenerationClass is not set") else
    if (mainSourceSet.isEmpty) skipping("no Java plugin in the project") else
    if (classesTask.isEmpty) skipping("no 'classes' task in the project") else
//    if (!classesTask.get.getDidWork) skipping("'classes' task didn't do work") else
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

  private def getMathJaxConfiguration: mathjax.Configuration = {
    def delimiters(property: Property[String]): Seq[mathjax.Configuration.Delimiters] =
      Seq(new mathjax.Configuration.Delimiters(property.get, property.get))

    mathjax.Configuration(
      font = mathJaxFont.get,
      extensions = mathJaxExtensions.get.asScala.toList,
      texDelimiters = delimiters(texDelimiter),
      texInlineDelimiters = delimiters(texInlineDelimiter),
      asciiMathDelimiters = delimiters(asciiMathDelimiter)
    )
  }
}
