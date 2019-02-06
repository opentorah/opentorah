package org.podval.docbook.gradle

import org.gradle.api.artifacts.Configuration
import org.gradle.api.{Action, DefaultTask}
import org.gradle.api.file.CopySpec
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
  @OutputDirectory @BeanProperty val finalOutputDirectoryRoot: File = layout.finalOutputDirectoryRoot
  @OutputDirectory @BeanProperty val intermediateOutputDirectoryRoot: File = layout.intermediateOutputDirectoryRoot

  @Input @BeanProperty val inputFileName: Property[String] =
    getProject.getObjects.property(classOf[String])

  @Input @BeanProperty val xslParameters: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @Input @BeanProperty val substitutions: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @Input @BeanProperty val outputFormats: ListProperty[String] =
    getProject.getObjects.listProperty(classOf[String])

  @TaskAction
  def docBook(): Unit = {
    val logger: Logger = new Logger.PluginLogger(getLogger)

    val docBookXslConfiguration: Configuration =
      getProject.getConfigurations.findByName(layout.docBookXslConfigurationName)

    // Unpack DocBook XSLT stylesheets
    if (!layout.docBookXslDirectory.exists) {
      logger.info(s"Preparing DocBook XSLT stylesheets")
      getProject.copy(new Action[CopySpec] {
        override def execute(copySpec: CopySpec): Unit = {
          copySpec
            .into(layout.explodeDocBookXslInto)
            .from(getProject.zipTree(docBookXslConfiguration.getSingleFile))
        }
      })
    }

    val saxon: Saxon = new Saxon(
      xslDirectory = layout.docBookXslDirectory,
      logger: Logger
    )

    val processorsToRun: List[DocBook2] = outputFormats.get.asScala.toList.map(DocBook2.forName)
    val processorNames: String = processorsToRun.map(_.name).mkString(", ")
    logger.info(s"Output formats selected: $processorNames")

    processorsToRun.foreach(_.run(
      layout = layout,
      saxon = saxon,
      inputFileName = inputFileName.get,
      xslParameters = xslParameters.get.asScala.toMap,
      substitutions = substitutions.get.asScala.toMap,
      project = getProject,
      logger = logger
    ))
  }
}
