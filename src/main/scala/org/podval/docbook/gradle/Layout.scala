package org.podval.docbook.gradle

import java.io.File

import org.gradle.api.Project

final class Layout(project: Project) {
  val sourceRootDirectory: File = new File(project.getProjectDir, "src/main")

  private def sourceDirectory(name: String): File = new File(sourceRootDirectory, name)

  val inputDirectory: File = sourceDirectory("docBook")
  def inputFile(inputFileName: String) = new File(inputDirectory, inputFileName + ".xml")

  val stylesheetDirectory: File = sourceDirectory("xsl")
  def stylesheetFile(finalOutputFormat: String) = new File(stylesheetDirectory, finalOutputFormat + ".xsl")

  val imagesDirectoryName: String = "images"
  val imagesDirectory: File = sourceDirectory(imagesDirectoryName)

  val cssDirectoryName: String = "css"
  val cssDirectory: File = sourceDirectory(cssDirectoryName)

  val fopConfigurationDirectory: File = sourceDirectory("fop")
  val fopConfigurationFile: File = new File(fopConfigurationDirectory, "fop.xconf")

  private val buildRootDirectory: File = project.getBuildDir
  private def buildDirectory(name: String): File = new File(buildRootDirectory, name)

  val dataDirectory: File = buildDirectory("data")

  private val explodeDocBookXslInto: File = buildDirectory("docBookXsl")
  val docBookXslDirectory: File = new File(explodeDocBookXslInto, "docbook")

  val finalOutputDirectoryRoot: File = buildDirectory("docBook")
  val intermediateOutputDirectoryRoot: File = buildDirectory("docBookTmp")
}
