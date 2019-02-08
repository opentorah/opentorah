package org.podval.docbook.gradle

import org.gradle.api.artifacts.Configuration
import org.gradle.api.{Action, DefaultTask}
import org.gradle.api.file.{CopySpec, FileCopyDetails, RelativePath}
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, TaskAction}
import java.io.{File, InputStream}
import javax.inject.Inject
import scala.beans.BeanProperty
import scala.collection.JavaConverters._

@Inject
class DocBookTask extends DefaultTask {
  val layouts: Layouts = Layouts.forProject(getProject)

  // Register inputs and outputs
  layouts.inputDirectories.foreach(getInputs.dir)
  layouts.outputDirectories.foreach(getOutputs.dir)

  @Input @BeanProperty val inputFileName: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val xslParameters: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

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
    val logger: Logger = new Logger.PluginLogger(getLogger)

    val processors: List[DocBook2] =
      getProcessors("docBook.outputFormats", outputFormats, DocBook2.forXslt1)

    val processors2: List[DocBook2] =
      getProcessors("docBook.outputFormats2", outputFormats2, DocBook2.forXslt2)

    logger.info(s"Output formats: ${getNames(processors)}")

    if (processors2.nonEmpty)
      logger.info(s"Output formats using XSLT 2.0: ${getNames(processors2)}")

    // Common configuration files.
    if (processors.nonEmpty || processors2.nonEmpty) {
      copyResource("css", "docBook.css", layouts.forXslt1.cssFile, logger)
      copyResource("fop", "fop.xconf", layouts.forXslt1.fopConfigurationFile, logger)
    }

    run(processors2, layouts.forXslt2, logger, Set("html", "fo"))

    run(processors, layouts.forXslt1, logger, Set("common", "common-html", "epub", "fo", "html"))
  }

  private def getProcessors(
    propertyName: String,
    property: ListProperty[String],
    supported: List[DocBook2]
  ): List[DocBook2] = {
    def forName(name: String): DocBook2 = {
      supported.find(processor => processor.name.equalsIgnoreCase(name)).getOrElse {
        throw new IllegalArgumentException(
          s"""Unsupported output format $name;
             |  supported formats are: ${getNames(supported)}""".stripMargin
        )
      }
    }

    Option(getProject.findProperty(propertyName))
      .map(_.toString.split(",").map(_.trim).toList.filter(_.nonEmpty))
      .getOrElse(property.get.asScala.toList)
      .map(forName)
  }

  private def getNames(processors: List[DocBook2]): String =
    "[" + processors.map(processor => "\"" + processor.name +"\"").mkString(", ") + "]"

  private def run(
    processors: List[DocBook2],
    layout: Layout,
    logger: Logger,
    customizations: Set[String]
  ): Unit = if (processors.nonEmpty) {
    // Write out default XSLT customizations.
    customizations.foreach { name: String =>
      copyResource(layout.stylesheetDirectoryName, name + ".xsl", layout.stylesheetFile(name), logger)
    }

    unpackDocBookXsl(layout, logger)

    processors.foreach(processor => run(processor, layout, logger))
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

  private def unpackDocBookXsl(layout: Layout, logger: Logger): Unit = {
    val directory: File = layout.docBookXslDirectory
    if (!directory.exists) {
      val name: String = if (layout.useDocBookXslt2) "XSLT 2.0" else "XSLT"
      logger.info(s"Preparing DocBook $name stylesheets")

      val configuration: Configuration = getProject.getConfigurations.findByName(layout.docBookXslConfigurationName)
      val zip: File = configuration.getSingleFile
      val toDrop: Int = layout.docBookXslArchiveSubdirectoryName.count(_ == '/') + 1
      getProject.copy(new Action[CopySpec] {
        override def execute(copySpec: CopySpec): Unit = {
          copySpec
            .into(directory)
            .from(getProject.zipTree(zip))
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

  private def run(processor: DocBook2, layout: Layout, logger: Logger): Unit = processor.run(
    layout = layout,
    inputFileName = inputFileName.get,
    xslParameters = xslParameters.get.asScala.toMap,
    substitutions = substitutions.get.asScala.toMap,
    epubEmbeddedFonts = epubEmbeddedFonts.get.asScala.toList,
    project = getProject,
    logger = logger
  )
}
