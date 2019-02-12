package org.podval.docbook.gradle

import org.gradle.api.{Action, DefaultTask, Project}
import org.gradle.api.artifacts.Configuration
import org.gradle.api.file.{CopySpec, FileCopyDetails, RelativePath}
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, Internal, TaskAction}
import java.io.{File, InputStream}
import scala.beans.BeanProperty
import scala.collection.JavaConverters._

class DocBookTask extends DefaultTask {

  private val layouts: Layouts = Layouts.forProject(getProject)

  private val logger: Logger = new Logger.PluginLogger(getLogger)

  // To let projects that use the plugin to not make assumptions about directory names
  @Internal def getOutputDirectory: File = layouts.forXslt1.outputDirectoryRoot

  // Register inputs
  val inputDirectories: Set[File] = Set(
    layouts.forXslt1.inputDirectory,
    layouts.forXslt1.cssDirectory,
    layouts.forXslt1.fopConfigurationDirectory,
    layouts.forXslt1.dataDirectory,
    layouts.forXslt1.stylesheetDirectory
  ) ++ Set(
    layouts.forXslt1.imagesDirectory,
    // Only stylesheetDirectory can be different for XSLT 1.0 and 2.0
    layouts.forXslt2.stylesheetDirectory
  ).filter(_.exists)

  inputDirectories.foreach { directory: File =>
    logger.info(s"Registering input directory $directory")
    directory.mkdirs()
    getOutputs.dir(directory)
  }

  // Register outputs
  Set(
    layouts.forXslt1.saxonOutputDirectoryRoot,
    layouts.forXslt1.outputDirectoryRoot,
    layouts.forXslt2.saxonOutputDirectoryRoot,
    layouts.forXslt2.outputDirectoryRoot
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
  def docBook(): Unit = {
    val processors: List[DocBook2] =
      DocBookTask.getProcessors("docBook.outputFormats", outputFormats, DocBook2.forXslt1, getProject)

    val processors2: List[DocBook2] =
      DocBookTask.getProcessors("docBook.outputFormats2", outputFormats2, DocBook2.forXslt2, getProject)

    logger.info(s"Output formats: ${DocBookTask.getNames(processors)}")

    if (processors2.nonEmpty)
      logger.info(s"Output formats using XSLT 2.0: ${DocBookTask.getNames(processors2)}")

    // Common configuration files.
    if (processors.nonEmpty || processors2.nonEmpty) {
      DocBookTask.copyResource("css", "docBook.css", layouts.forXslt1.cssFile, logger)
      DocBookTask.copyResource("fop", "fop.xconf", layouts.forXslt1.fopConfigurationFile, logger)
    }

    run(processors2, layouts.forXslt2, logger, Set("html", "fo"))

    run(processors, layouts.forXslt1, logger, Set("common", "common-html", "epub",  "epub3", "fo", "html"))
  }

  private def run(
    processors: List[DocBook2],
    layout: Layout,
    logger: Logger,
    customizations: Set[String]
  ): Unit = if (processors.nonEmpty) {
    // Write out default XSLT customizations.
    customizations.foreach { name: String =>
      DocBookTask.copyResource(layout.stylesheetDirectoryName, name + ".xsl", layout.stylesheetFile(name), logger)
    }

    DocBookTask.unpackDocBookXsl(layout, getProject, logger)

    processors.foreach(processor => run(processor, layout, logger))
  }

  private def run(processor: DocBook2, layout: Layout, logger: Logger): Unit = processor.run(
    layout = layout,
    isJEuclidEnabled = isJEuclidEnabled.get,
    inputFileName = inputFileName.get,
    xslParameters = xslParameters.get.asScala.toMap,
    substitutions = substitutions.get.asScala.toMap,
    epubEmbeddedFonts = epubEmbeddedFonts.get.asScala.toList,
    project = getProject,
    logger = logger
  )
}

object DocBookTask {

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
             |  supported formats are: ${DocBookTask.getNames(supported)}""".stripMargin
        )
      }
    }

    Option(project.findProperty(propertyName))
      .map(_.toString.split(",").map(_.trim).toList.filter(_.nonEmpty))
      .getOrElse(property.get.asScala.toList)
      .map(forName)
  }

  private def copyResource(directory: String, name: String, file: File, logger: Logger): Unit = {
    if (file.exists()) {
      logger.info(s"Skipping configuration resource $name: file $file already exists")
    } else {
      logger.info(s"Copying configuration resource $name to $file")
      file.getParentFile.mkdirs

      val is: InputStream = getClass.getResourceAsStream(s"/$directory/$name")
      if (is == null) {
        val message: String = s"Configuration resource not found: $name"
        logger.error(message)
        throw new IllegalArgumentException(message)
      }
      java.nio.file.Files.copy(is, file.toPath)
    }
  }

  private def getNames(processors: List[DocBook2]): String =
    "[" + processors.map(processor => "\"" + processor.name +"\"").mkString(", ") + "]"

  def unpackDocBookXsl(layout: Layout, project: Project, logger: Logger): Unit = {
    val directory: File = layout.docBookXslDirectory
    if (!directory.exists) {
      val name: String = if (layout.useDocBookXslt2) "XSLT 2.0" else "XSLT"
      logger.info(s"Preparing DocBook $name stylesheets")

      val configuration: Configuration = project.getConfigurations.findByName(layout.docBookXslConfigurationName)
      val zip: File = configuration.getSingleFile
      val toDrop: Int = layout.docBookXslArchiveSubdirectoryName.count(_ == '/') + 1
      project.copy(new Action[CopySpec] {
        override def execute(copySpec: CopySpec): Unit = {
          copySpec
            .into(directory)
            .from(project.zipTree(zip))
            // following 7 lines of code deal with extracting just the "docbook" directory;
            // this should become easier in Gradle 5.3, see:
            // https://github.com/gradle/gradle/issues/1108
            // https://github.com/gradle/gradle/pull/5405
            .include(layout.docBookXslArchiveSubdirectoryName +  "/**")
            .eachFile(new Action[FileCopyDetails] {
              override def execute(file: FileCopyDetails): Unit = {
                file.setRelativePath(new RelativePath(true, file.getRelativePath.getSegments.drop(toDrop): _*))
              }
            })
            .setIncludeEmptyDirs(false)
        }
      })
    }
  }
}
