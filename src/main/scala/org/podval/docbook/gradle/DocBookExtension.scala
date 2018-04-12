package org.podval.docbook.gradle

import java.io.File

import org.gradle.api.Project
import org.gradle.api.provider.Property

class DocBookExtension(project: Project) {
  val inputFileName: Property[String] = project.getObjects.property(classOf[String])
  def getInputFileName: Property[String] = inputFileName // for Gradle DSL
  inputFileName.set("index")

  val dataDirectory: Property[File] = project.getObjects.property(classOf[File])
  def getDataDirectory: Property[File] = dataDirectory // for Gradle DSL
  dataDirectory.set(DocBookPlugin.buildDirectory(project, "data"))

  val xslParameters: Property[java.util.Map[String, String]] = project.getObjects.property(classOf[java.util.Map[String, String]])
  def getXslParameters: Property[java.util.Map[String, String]] = xslParameters
}
