package org.podval.docbook.gradle

import org.gradle.api.Project
import java.io.File

final class Layout(project: Project) {
  val docBookXslConfigurationName: String = "docBookXsl"

  private val sourceRootDirectory: File = new File(project.getProjectDir, "src/main")
  private def sourceDirectory(name: String): File = new File(sourceRootDirectory, name)

  val inputDirectory: File = sourceDirectory("docBook")
  def inputFile(inputFileName: String) = new File(inputDirectory, inputFileName + ".xml")

  val stylesheetDirectory: File = sourceDirectory("xsl")
  def stylesheetFile(name: String) = new File(stylesheetDirectory, name + ".xsl")

  val imagesDirectoryName: String = "images"
  val imagesDirectory: File = sourceDirectory(imagesDirectoryName)

  val cssDirectoryName: String = "css"
  val cssDirectory: File = sourceDirectory(cssDirectoryName)
  val cssFileName: String = "docBook.css"
  val cssFile: File = new File(cssDirectory, cssFileName)

  val fopConfigurationDirectory: File = sourceDirectory("fop")
  val fopConfigurationFile: File = new File(fopConfigurationDirectory, "fop.xconf")

  private val buildRootDirectory: File = project.getBuildDir
  private def buildDirectory(name: String): File = new File(buildRootDirectory, name)

  val docBookXslDirectory: File = buildDirectory("docBookXsl")
  val dataDirectory: File = buildDirectory("data")
  val outputDirectoryRoot: File = buildDirectory("docBook")
  val saxonOutputDirectoryRoot: File = buildDirectory("docBookTmp")
}
