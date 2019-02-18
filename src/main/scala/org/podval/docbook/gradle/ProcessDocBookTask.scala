package org.podval.docbook.gradle

import org.gradle.api.{DefaultTask, Project}
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, Internal, TaskAction}
import java.io.File
import scala.beans.BeanProperty
import scala.collection.JavaConverters._

class ProcessDocBookTask extends DefaultTask {

  private val layout: Layout = Layout.forProject(getProject)

  private val logger: Logger = new Logger.PluginLogger(getLogger)

  // To let projects that use the plugin to not make assumptions about directory names:
  @Internal def getOutputDirectory: File = layout.forXslt1.outputDirectoryRoot
  @Internal def getOutputDirectory2: File = layout.forXslt2.outputDirectoryRoot

  // Register inputs
  val inputDirectories: Set[File] = Set(
    layout.inputDirectory,
    layout.cssDirectory,
    layout.fopConfigurationDirectory,
    layout.dataDirectory,
    layout.forXslt1.stylesheetDirectory
  ) ++ Set(
    layout.imagesDirectory,
    layout.forXslt2.stylesheetDirectory
  ).filter(_.exists)

  inputDirectories.foreach { directory: File =>
    logger.info(s"Registering input directory $directory")
    directory.mkdirs()
    getOutputs.dir(directory)
  }

  // Register outputs
  Set(
    layout.forXslt1.saxonOutputDirectoryRoot,
    layout.forXslt1.outputDirectoryRoot,
    layout.forXslt2.saxonOutputDirectoryRoot,
    layout.forXslt2.outputDirectoryRoot
  ).foreach { directory: File =>
    Util.deleteRecursively(directory)
    logger.info(s"Registering output directory $directory")
    getOutputs.dir(directory)
  }

  @Input @BeanProperty val isJEuclidEnabled: Property[Boolean] =
    getProject.getObjects.property(classOf[Boolean])

  @Input @BeanProperty val inputFileName: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val xslParameters: MapProperty[String, Object] =
    getProject.getObjects.mapProperty(classOf[String], classOf[Object])

  @Input @BeanProperty val substitutions: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @Input @BeanProperty val outputFormats: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val outputFormats2: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @Input @BeanProperty val epubEmbeddedFonts: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @TaskAction
  def processDocBook(): Unit = {
    val processors: List[DocBook2] =
      getProcessors("docBook.outputFormats", outputFormats, DocBook2.forXslt1, getProject)

    val processors2: List[DocBook2] =
      getProcessors("docBook.outputFormats2", outputFormats2, DocBook2.forXslt2, getProject)

    logger.info(s"Output formats: ${getNames(processors)}")

    if (processors2.nonEmpty)
      logger.info(s"Output formats using XSLT 2.0: ${getNames(processors2)}")

    val resolver: Resolver = new Resolver(layout.catalogFile,  logger)

    run(processors, false, resolver)
    run(processors2, true, resolver)
  }

  private def getProcessors(
     propertyName: String,
     property: ListProperty[String],
     supported: List[DocBook2],
     project: Project,
  ): List[DocBook2] = {
    def forName(name: String): DocBook2 = {
      supported.find(processor => processor.name.equalsIgnoreCase(name)).getOrElse {
        throw new IllegalArgumentException(
          s"""Unsupported output format $name;
             |  supported formats are: ${getNames(supported)}""".stripMargin
        )
      }
    }

    Option(project.findProperty(propertyName))
      .map(_.toString.split(",").map(_.trim).toList.filter(_.nonEmpty))
      .getOrElse(property.get.asScala.toList)
      .map(forName)
  }

  private def run(
    processors: List[DocBook2],
    useDocBookXslt2: Boolean,
    resolver: Resolver
  ): Unit = processors.foreach { _.run(
    layout = layout,
    useDocBookXslt2 = useDocBookXslt2,
    isJEuclidEnabled = isJEuclidEnabled.get,
    inputFileName = inputFileName.get,
    xslParameters = xslParameters.get.asScala.toMap,
    substitutions = substitutions.get.asScala.toMap,
    epubEmbeddedFonts = epubEmbeddedFonts.get.asScala.toList,
    project = getProject,
    resolver = resolver,
    logger = logger
  )}

  private def getNames(processors: List[DocBook2]): String =
    "[" + processors.map(processor => "\"" + processor.name +"\"").mkString(", ") + "]"
}
