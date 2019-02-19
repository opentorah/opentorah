package org.podval.docbook.gradle

import java.io.File

import org.gradle.api.Project

final class Layout(projectDir: File, buildDir: File) {

  private def buildDirRelative: String =
    if (buildDir.getParentFile == projectDir) buildDir.getName
    else throw new IllegalArgumentException("buildDir that is not a child of projectDir is not yet supported! ")

  def docBookXslDirectory(docBookXslDirectoryName: String): File = buildDirectory(docBookXslDirectoryName)
  def docBookXslDirectoryRelative(docBookXslDirectoryName: String): String = buildDirectoryRelative(docBookXslDirectoryName)

  private def sourceDir: File = new File(projectDir, "src/main")
  private def sourceDirectory(name: String): File = new File(sourceDir, name)

  private def inputDirectoryName: String = "docBook"
  def inputDirectory: File = sourceDirectory(inputDirectoryName)
  def inputFile(inputFileName: String): File = new File(inputDirectory, inputFileName + ".xml")

  def cssDirectoryName: String = "css"
  def cssDirectory: File = sourceDirectory(cssDirectoryName)
  def cssFile(name: String): File = new File(cssDirectory, name + ".css")
  def cssFileRelative(name: String): String = cssDirectoryName + "/" + name + ".css"

  def imagesDirectoryName: String = "images"
  def imagesDirectory: File = sourceDirectory(imagesDirectoryName)

  private def fopConfigurationDirectoryName: String = "fop"
  def fopConfigurationDirectory: File = sourceDirectory(fopConfigurationDirectoryName)

  private def fopConfigurationFileName: String = "fop.xconf"
  def fopConfigurationFile: File = new File(fopConfigurationDirectory, fopConfigurationFileName)

  def mainStylesheet(name: String): String = name + ".xsl"
  def customStylesheet(name: String): String = name + "-custom.xsl"
  def paramsStylesheet(name: String): String = name + "-params.xsl"

  def buildDirectory(name: String): File = new File(buildDir, name)
  def buildDirectoryRelative(name: String): String = s"$buildDirRelative/$name/"

  private def dataDirectoryName: String = "data"
  def dataDirectory: File = buildDirectory(dataDirectoryName)
  def dataDirectoryRelative: String = buildDirectoryRelative(dataDirectoryName)

  def catalogGroupBase: String = "../../.."
  private def xmlDirectory: File = sourceDirectory("xml")
  def xmlFile(name: String): File = new File(xmlDirectory, name)
  def substitutionsDtdFileName: String = "substitutions.dtd"

  private def catalogFileName: String = "catalog.xml"
  def catalogFile: File = xmlFile(catalogFileName)
  def catalogCustomFileName: String = "catalog-custom.xml"

  private def outputDirectoryName: String = "docBook"
  def outputDirectoryRoot: File = buildDirectory(outputDirectoryName)

  private def saxonOutputDirectoryName: String = "docBookTmp"
  def saxonOutputDirectoryRoot: File = buildDirectory(saxonOutputDirectoryName)

  private def stylesheetDirectoryName: String = "xsl"
  def stylesheetDirectory: File = sourceDirectory(stylesheetDirectoryName)
  def stylesheetFile(name: String) = new File(stylesheetDirectory, name)
}

object Layout {
  def forProject(project: Project): Layout =
    new Layout(project.getProjectDir, project.getBuildDir)

  def forRoot(root: File): Layout =
    new Layout(root, new File(root, "build"))
}
