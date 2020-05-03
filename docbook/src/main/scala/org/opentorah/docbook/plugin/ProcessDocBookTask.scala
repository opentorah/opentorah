package org.opentorah.docbook.plugin

import java.io.File
import java.net.URI
import org.gradle.api.logging.Logger
import org.gradle.api.DefaultTask
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, Internal, SourceSet, TaskAction}
import org.opentorah.docbook.section.{DocBook2, Section}
import org.opentorah.fop.{Fop, FopFonts}
import org.opentorah.mathjax
import org.opentorah.mathjax.MathJax
import org.opentorah.node.NodeDistribution
import org.opentorah.util.Collections.mapValues
import org.opentorah.util.{Files, Gradle}
import org.opentorah.xml.{Namespace, Resolver}
import scala.beans.BeanProperty
import scala.collection.JavaConverters._

class ProcessDocBookTask extends DefaultTask {

  private val layout: Layout = Layout.forProject(getProject)
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
    layout.imagesDirectory,
  ).filter(_.exists)

  // Register inputs

  getProcessInputDirectories.foreach(registerInputDirectory)

  // Note: classesDirs use sourceSets, which are only available after project is evaluated;
  // with code in Scala only, input directory "build/classes/java/main" doesn't exist (which is a Gradle error), so I
  // register "build/classes/" (grandparent of all classesDirs) as input directory
  // (is there a simpler way of getting it?).
  // TODO update plugin instructions.
  // Data generation class doesn't have to reside in the same project where DocBook plugin is configured:
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

  @Input @BeanProperty val processMathJaxEscapes: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @TaskAction
  def processDocBook(): Unit = {
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

    val processors: List[DocBook2] =
      Option(getProject.findProperty("docBook.outputFormats"))
        .map(_.toString.split(",").map(_.trim).toList.filter(_.nonEmpty))
        .getOrElse(outputFormats.get.asScala.toList)
        .map(DocBook2.forName)

    info(s"Output formats: ${DocBook2.getNames(processors)}")

    val sections: Map[Section, Map[String, String]] =
      for ((sectionName: String, sectionParameters: Map[String, String]) <-
           mapValues(parameters.get.asScala.toMap)(_.asScala.toMap))
        yield (Section.forName(sectionName), sectionParameters)

    val unusedSections: Set[Section] =
      sections.keySet -- processors.flatMap(_.parameterSections).toSet

    if (unusedSections.nonEmpty)
      info(s"Unused parameter sections: ${unusedSections.map(_.name).mkString(", ")}")

    require(!isMathJaxEnabled.get || !isJEuclidEnabled.get)

    val mathJaxConfiguration: mathjax.Configuration = getMathJaxConfiguration

    Stylesheets.xslt1.unpack(xslt1version.get, getProject, layout)
    Stylesheets.xslt2.unpack(xslt2version.get, getProject, layout)

    val substitutionsMap: Map[String, String] = substitutions.get.asScala.toMap

    Files.write(
      file = layout.fopConfigurationFile,
      replace = false,
      content = Fop.defaultConfigurationFile
    )

    Files.write(
      file = layout.xmlFile(layout.substitutionsDtdFileName),
      replace = true,
      content = substitutionsMap.toSeq.map {
        case (name: String, value: String) => s"""<!ENTITY $name "$value">\n"""
      }.mkString
    )

    Files.write(
      file = layout.catalogFile,
      replace = true,
      content = Write.xmlCatalog(layout)
    )

    Files.write(
      file = layout.xmlFile(layout.catalogCustomFileName),
      replace = false,
      content = Write.catalogCustomization
    )

    val cssFileName: String = Files.dropAllowedExtension(cssFile.get, "css")

    Files.write(
      file = layout.cssFile(cssFileName),
      replace = false,
      content =
        s"""@namespace xml "${Namespace.Xml.uri}";
           |""".stripMargin
    )

    for ((name: String, _ /*prefixed*/: Boolean) <- inputDocuments) Files.write(
      file = layout.inputFile(name),
      replace = false,
      content = Write.defaultInputFile
    )

    val fontFamilyNames: List[String] = epubEmbeddedFonts.get.asScala.toList
    val epubEmbeddedFontsUris: List[URI] = FopFonts.getFiles(layout.fopConfigurationFile, fontFamilyNames)
    val epubEmbeddedFontsString: String = epubEmbeddedFontsUris.map(uri => new File(uri.getPath).getAbsolutePath).mkString(", ")
    info(s"Fop.getFontFiles(${fontFamilyNames.mkString(", ")}) = $epubEmbeddedFontsString.")

    for {
      docBook2: DocBook2 <- DocBook2.all
      (documentName: String, prefixed: Boolean) <- inputDocuments
    } Files.write(
      file = layout.stylesheetFile(layout.forDocument(prefixed, documentName).mainStylesheet(docBook2)),
      replace = true,
      content = Write.mainStylesheet(
        docBook2,
        prefixed,
        documentName,
        cssFileName,
        epubEmbeddedFontsString,
        mathJaxConfiguration,
        layout
      )
    )

    for (docBook2: DocBook2 <- DocBook2.all) Files.write(
      file = layout.stylesheetFile(layout.paramsStylesheet(docBook2)),
      replace = true,
      content = Write.paramsStylesheet(docBook2, sections, getProject.getLogger.isInfoEnabled)
    )

    for (section: Section <- Section.all) Files.write(
      file = layout.stylesheetFile(layout.customStylesheet(section)),
      replace = false,
      content = Write.customStylesheet(layout, section)
    )

    generateData()

    val mathJax: Option[MathJax] = if (!processors.exists(_.isPdf) || !isMathJaxEnabled.get) None
    else Some(MathJax.get(
      getProject,
      nodeParent = layout.nodeRoot,
      nodeVersion = NodeDistribution.defaultVersion,
      overwriteNode = false,
      nodeModulesParent = layout.nodeRoot,
      overwriteMathJax = false,
      j2v8Parent = if (!useJ2V8.get) None else Some(layout.j2v8LibraryDirectory),
      configuration = mathJaxConfiguration))

    val processDocBook: ProcessDocBook = new ProcessDocBook(
      getProject,
      // In processing instructions and CSS, substitute xslParameters also - because why not?
      substitutions = sections.values.toList.flatten.toMap ++ substitutionsMap,
      resolver = new Resolver(layout.catalogFile),
      isJEuclidEnabled.get,
      mathJax,
      layout
    )

    for {
      docBook2: DocBook2 <- processors
      (documentName: String, prefixed: Boolean) <- inputDocuments
    } {
      getProject.getLogger.lifecycle(s"DocBook: processing '$documentName' to ${docBook2.name}.")
      processDocBook.run(
        docBook2,
        prefixed,
        documentName
      )
    }
  }

  private def getDocumentName(string: String): Option[String] =
    if (string.isEmpty) None else Some(Files.dropAllowedExtension(string, "xml"))

  private def generateData(): Unit = {
    val mainClass: String = dataGeneratorClass.get
    val mainSourceSet: Option[SourceSet] = Gradle.mainSourceSet(getProject)
    val dataDirectory: File = layout.dataDirectory

    def skipping(message: String): Unit = getProject.getLogger.lifecycle(s"Skipping DocBook data generation: $message")
    //    val classesTask: Option[Task] = Gradle.getClassesTask(getProject)
    if (mainClass.isEmpty) info("Skipping DocBook data generation: dataGenerationClass is not set") else
// TODO maybe instead of the Java plugin use special configuration (docBook :))?
    if (mainSourceSet.isEmpty) skipping("no Java plugin in the project") else
// TODO update plugin instructions.
// Data generation class doesn't have to reside in the same project where DocBook plugin is configured:
//    if (classesTask.isEmpty) skipping("no 'classes' task in the project") else
//    if (!didWork(classesTask.get)) skipping("'classes' task didn't do work") else
    {
      info(s"Running DocBook data generator $mainClass into $dataDirectory")
      Gradle.javaexec(
        getProject,
        mainClass,
        mainSourceSet.get.getRuntimeClasspath,
        dataDirectory.toString
       )
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
      asciiMathDelimiters = delimiters(asciiMathDelimiter),
      processEscapes = processMathJaxEscapes.get
    )
  }
}
