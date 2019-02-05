package org.podval.docbook.gradle

import java.io.File

import javax.inject.Inject
import org.gradle.api.DefaultTask
import org.gradle.api.provider.{ListProperty, MapProperty, Property}
import org.gradle.api.tasks.{Input, InputDirectory, OutputDirectory, TaskAction}

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
    DocBook2.process(
      outputFormats = outputFormats.get.asScala.toList,
      layout = layout,
      inputFileName = inputFileName.get,
      xslParameters = xslParameters.get.asScala.toMap,
      substitutions = substitutions.get.asScala.toMap,
      project = getProject,
      logger = new Logger.PluginLogger(getLogger)
    )
  }
}
