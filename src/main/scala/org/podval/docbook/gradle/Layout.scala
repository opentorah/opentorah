package org.podval.docbook.gradle

import java.io.File

import org.gradle.api.Project
import org.podval.docbook.gradle.section.{DocBook2, Section}

final class Layout(projectDir: File, buildDir: File) {
  // src/main/
  private def sourceDir: File = new File(projectDir, "src/main")
  private def sourceDirectory(name: String): File = new File(sourceDir, name)

  // src/main/docBook/
  private def inputDirectoryName: String = "docBook"
  def inputDirectory: File = sourceDirectory(inputDirectoryName)
  def inputFile(inputFileName: String): File = new File(inputDirectory, inputFileName + ".xml")

  // src/main/css/
  def cssDirectoryName: String = "css"
  def cssDirectory: File = sourceDirectory(cssDirectoryName)
  def cssFile(name: String): File = new File(cssDirectory, name + ".css")
  def cssFileRelativeToOutputDirectory(name: String): String = cssDirectoryName + "/" + name + ".css"

  // src/main/images/
  def imagesDirectoryName: String = "images"
  def imagesDirectory: File = sourceDirectory(imagesDirectoryName)

  // src/main/fop/
  private def fopConfigurationDirectoryName: String = "fop"
  def fopConfigurationDirectory: File = sourceDirectory(fopConfigurationDirectoryName)
  private def fopConfigurationFileName: String = "fop.xconf"
  def fopConfigurationFile: File = new File(fopConfigurationDirectory, fopConfigurationFileName)

  // src/main/xsl/
  def mainStylesheet(section: Section, prefixed: Boolean, documentName: String): String =
    section.name + (if (!prefixed) "" else "-" + documentName) + ".xsl"
  def customStylesheet(section: Section): String = section.name + "-custom.xsl"
  def paramsStylesheet(section: Section): String = section.name + "-params.xsl"
  private def stylesheetDirectoryName: String = "xsl"
  def stylesheetDirectory: File = sourceDirectory(stylesheetDirectoryName)
  def stylesheetFile(name: String) = new File(stylesheetDirectory, name)

  // src/main/xml/
  def catalogGroupBase: String = "../../.."  // to get to the project root
  private def xmlDirectory: File = sourceDirectory("xml")
  def xmlFile(name: String): File = new File(xmlDirectory, name)
  def substitutionsDtdFileName: String = "substitutions.dtd"
  private def catalogFileName: String = "catalog.xml"
  def catalogFile: File = xmlFile(catalogFileName)
  def catalogCustomFileName: String = "catalog-custom.xml"

  // build/
  private def buildDirRelative: String =
    if (buildDir.getParentFile == projectDir) buildDir.getName
    else throw new IllegalArgumentException("buildDir that is not a child of projectDir is not yet supported! ")

  def buildDirectory(name: String): File = new File(buildDir, name)
  def buildDirectoryRelative(name: String): String = s"$buildDirRelative/$name/"

  // build/docBookXslt[2]
  def docBookXslDirectory(docBookXslDirectoryName: String): File = buildDirectory(docBookXslDirectoryName)
  def docBookXslDirectoryRelative(docBookXslDirectoryName: String): String = buildDirectoryRelative(docBookXslDirectoryName)

  // build/data
  private def dataDirectoryName: String = "data"
  def dataDirectory: File = buildDirectory(dataDirectoryName)
  def dataDirectoryRelative: String = buildDirectoryRelative(dataDirectoryName)

  // build/docBook
  // build/docBookTmp

  private def outputRootName: String = "docBook"
  def outputRoot: File = buildDirectory(outputRootName)

  private def intermediateRootName: String = "docBookTmp"
  def intermediateRoot: File = buildDirectory(intermediateRootName)

  private def saxonOutputDirectoryName(docBook2: DocBook2): String =
    if (docBook2.usesIntermediate) docBook2.intermediateDirectoryName else docBook2.name

  // Relative base.dir doesn't work for html2 (is it even needed?) and in test projects
  private val useRelativeBaseDir: Boolean = false
  def baseDir(
    docBook2: DocBook2,
    prefixed: Boolean,
    documentName: String
  ): String =
    if (!useRelativeBaseDir) saxonOutputDirectory(docBook2, prefixed, documentName).getAbsolutePath + "/" else {
      val directoryName: String = if (docBook2.usesIntermediate) intermediateRootName else outputRootName
      val directoryNameEffective: String = if (!prefixed) directoryName else s"$directoryName/$documentName"
      buildDirectoryRelative(s"$directoryNameEffective/${saxonOutputDirectoryName(docBook2)}")
    }

  def saxonOutputDirectory(
    docBook2: DocBook2,
    prefixed: Boolean,
    documentName: String
  ): File = {
    val saxonRoot: File = if (docBook2.usesIntermediate) intermediateRoot else outputRoot
    val saxonRootEffective: File = if (!prefixed) saxonRoot else new File(saxonRoot, documentName)
    new File(
      saxonRootEffective,
      saxonOutputDirectoryName(docBook2)
    )
  }

  def outputDirectory(docBook2: DocBook2): File = new File(outputRoot, docBook2.name)

  def saxonOutputFile(
    docBook2: DocBook2,
    prefixed: Boolean,
    inputFileName: String
  ): File = outputFile(
    docBook2,
    saxonOutputDirectory(docBook2, prefixed, inputFileName),
    inputFileName,
    if (docBook2.usesIntermediate) docBook2.intermediateFileExtension else docBook2.outputFileExtension
  )

  def outputFile(
    docBook2: DocBook2,
    inputFileName: String
  ): File =
    outputFile(docBook2, outputDirectory(docBook2), inputFileName, docBook2.outputFileExtension)

  private def outputFile(
    docBook2: DocBook2,
    directory: File,
    inputFileName: String,
    extension: String
  ): File =
    new File(directory, docBook2.rootFilename(inputFileName) + "." + extension)
}

object Layout {
  def forProject(project: Project): Layout =
    new Layout(project.getProjectDir, project.getBuildDir)

  def forRoot(root: File): Layout =
    new Layout(root, new File(root, "build"))
}
