package org.podval.docbook.gradle

import org.gradle.api.artifacts.Configuration
import org.gradle.api.{Action, DefaultTask}
import org.gradle.api.file.{CopySpec, FileCopyDetails, RelativePath}
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, InputDirectory, OutputDirectory, TaskAction}
import java.io.File
import javax.inject.Inject
import scala.beans.BeanProperty
import scala.collection.JavaConverters._

@Inject
class DocBookTask extends DefaultTask {
  val layout = new Layout(getProject)

  @InputDirectory @BeanProperty val inputDirectory: File = layout.inputDirectory
  @InputDirectory @BeanProperty val stylesheetDirectory: File = layout.stylesheetDirectory
  @InputDirectory @BeanProperty val dataDirectory: File = layout.dataDirectory
  @InputDirectory @BeanProperty val imagesDirectory: File = layout.imagesDirectory
  @InputDirectory @BeanProperty val cssDirectory: File = layout.cssDirectory
  @InputDirectory @BeanProperty val fopConfigurationDirectory: File = layout.fopConfigurationDirectory
  @OutputDirectory @BeanProperty val outputDirectoryRoot: File = layout.outputDirectoryRoot
  @OutputDirectory @BeanProperty val saxonOutputDirectoryRoot: File = layout.saxonOutputDirectoryRoot

  @Input @BeanProperty val inputFileName: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val xslParameters: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @Input @BeanProperty val substitutions: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @Input @BeanProperty val outputFormats: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @BeanProperty val epubEmbeddedFonts: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  // TODO this has to come from the extension
  val useDocBookXslt20: Boolean = false

  @TaskAction
  def docBook(): Unit = {
    val logger: Logger = new Logger.PluginLogger(getLogger)

    // Unpack DocBook XSLT stylesheets
    unpackDocBookXsl(
      name = "XSLT",
      configurationName = layout.docBookXslConfigurationName,
      directory = layout.docBookXslDirectory(false),
      archiveSubdirectoryName = "docbook",
      logger = logger
    )

    if (useDocBookXslt20) {
      unpackDocBookXsl(
        name = "XSLT 2.0",
        configurationName = layout.docBookXsl2ConfigurationName,
        directory = layout.docBookXslDirectory(true),
        archiveSubdirectoryName = "xslt",
        logger = logger
      )
    }

    val processorsToRun: List[DocBook2] =
      Option(getProject.findProperty("docBook.outputFormats"))
        .map(_.toString.split(",").map(_.trim).toList.filter(_.nonEmpty))
        .getOrElse(outputFormats.get.asScala.toList)
        .map(DocBook2.forName)

    val processorNames: String = processorsToRun.map(processor => "\"" + processor.name +"\"").mkString(", ")
    logger.info(s"Output formats selected: [$processorNames]")

    processorsToRun.foreach(_.run(
      layout = layout,
      inputFileName = inputFileName.get,
      xslParameters = xslParameters.get.asScala.toMap,
      substitutions = substitutions.get.asScala.toMap,
      epubEmbeddedFonts = epubEmbeddedFonts.get.asScala.toList,
      useDocBookXslt20 = useDocBookXslt20,
      project = getProject,
      logger = logger
    ))
  }


  private def unpackDocBookXsl(
    name: String,
    configurationName: String,
    directory: File,
    archiveSubdirectoryName: String,
    logger: Logger
  ): Unit = {
    if (!directory.exists) {
      logger.info(s"Preparing DocBook $name stylesheets")

      val configuration: Configuration = getProject.getConfigurations.findByName(configurationName)

      getProject.copy(new Action[CopySpec] {
        override def execute(copySpec: CopySpec): Unit = {
          copySpec
            .into(directory)
            .from(getProject.zipTree(configuration.getSingleFile))
            // following 7 lines of code deal with extracting just the "docbook" directory;
            // this should become easier in Gradle 5.3, see:
            // https://github.com/gradle/gradle/issues/1108
            // https://github.com/gradle/gradle/pull/5405
            .include(archiveSubdirectoryName +  "/**")
            .eachFile(new Action[FileCopyDetails] {
              override def execute(file: FileCopyDetails): Unit = {
                file.setRelativePath(new RelativePath(true, file.getRelativePath.getSegments.drop(1): _*))
              }
            })
            .setIncludeEmptyDirs(false)
        }
      })
    }
  }
}
