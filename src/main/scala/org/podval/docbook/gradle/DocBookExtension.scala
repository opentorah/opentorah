package org.podval.docbook.gradle

import java.io.File

import org.gradle.api.Project
import org.gradle.api.provider.{Property, MapProperty}

class DocBookExtension(project: Project) {
  val inputFileName: Property[String] = project.getObjects.property(classOf[String])
  def getInputFileName: Property[String] = inputFileName // for Gradle DSL
  inputFileName.set("index")

  val dataDirectory: Property[File] = project.getObjects.property(classOf[File])
  def getDataDirectory: Property[File] = dataDirectory // for Gradle DSL
  dataDirectory.set(Locations.dataDirectory(project))

  val imagesDirectory: Property[File] = project.getObjects.property(classOf[File])
  def getImagesDirectory: Property[File] = imagesDirectory // for Gradle DSL
  imagesDirectory.set(Locations.imagesDirectory(project))

  val xslParameters: MapProperty[String, String] = project.getObjects.mapProperty(classOf[String], classOf[String])
  def getXslParameters: MapProperty[String, String] = xslParameters
}
