package org.podval.docbook.gradle

import org.gradle.api.DefaultTask
import org.gradle.api.provider.{MapProperty, Property}
import org.gradle.api.tasks.{Input, InputDirectory, InputFile, OutputFile, TaskAction}
import java.io.File

import org.podval.docbook.gradle

import scala.beans.BeanProperty
import scala.collection.JavaConverters._

class SaxonTask extends DefaultTask {
  @InputFile @BeanProperty val inputFile: Property[File] =
    getProject.getObjects.property(classOf[File])

  @InputFile @BeanProperty val stylesheetFile: Property[File] =
    getProject.getObjects.property(classOf[File])

  @Input @BeanProperty val xslParameters: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @Input @BeanProperty val entities: MapProperty[String, String] =
    getProject.getObjects.mapProperty(classOf[String], classOf[String])

  @InputDirectory @BeanProperty val xslDirectory: Property[File] =
    getProject.getObjects.property(classOf[File])

  @InputDirectory @BeanProperty val dataDirectory: Property[File] =
    getProject.getObjects.property(classOf[File])

  @InputDirectory @BeanProperty val imagesDirectory: Property[File] =
    getProject.getObjects.property(classOf[File])

  @OutputFile @BeanProperty val outputFile: Property[File] =
    getProject.getObjects.property(classOf[File])

  @TaskAction
  def saxon(): Unit = Saxon(
    xslDirectory = xslDirectory.get,
    xslParameters = xslParameters.get.asScala.toMap,
    entities = entities.get.asScala.toMap,
    imagesDirectoryName = imagesDirectory.get.getName,
    dataDirectory = dataDirectory.get,
    evaluator = getEvaluator,
    logger = new gradle.Logger.PluginLogger(getLogger)
  ).run(
    inputFile = inputFile.get,
    stylesheetFile = stylesheetFile.get,
    outputFile = outputFile.get
  )

  private def getEvaluator: Evaluator = new Evaluator {
    override def eval(expression: String): Option[AnyRef] =
      Option(getProject.findProperty(expression))
  }
}
