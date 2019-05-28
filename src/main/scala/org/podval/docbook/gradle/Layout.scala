package org.podval.docbook.gradle

import java.io.File

import org.gradle.api.Project
import org.podval.docbook.gradle.section.{DocBook2, Section}

final class Layout(projectDir: File, buildDir: File) {
  // I do not see any point in caching anything under `.gradle`:
  // - Gradle caches artifacts elsewhere;
  // - NPM caches packages that it retrieves.
  // If there turns out a point in this, do it for both Node and DocBook.
  // private def cacheDirectory(name: String): File = new File(new File(projectDir, ".gradle"), name)

  // src/main/
  private def sourceDir: File = new File(new File(projectDir, "src"), "main")
  private def sourceDirectory(name: String): File = new File(sourceDir, name)

  // src/main/docBook/
  private def inputDirectoryName: String = "docBook"
  def inputDirectory: File = sourceDirectory(inputDirectoryName)
  def inputFile(inputFileName: String): File = new File(inputDirectory, inputFileName + ".xml")

  // src/main/css/
  def cssDirectoryName: String = "css"
  def cssDirectory: File = sourceDirectory(cssDirectoryName)
  def cssFile(name: String): File = new File(cssDirectory, name + ".css")
  def cssFileRelativeToOutputDirectory(name: String): String = cssDirectoryName + "/" + name + ".css" // TODO file separator!

  // src/main/images/
  def imagesDirectoryName: String = "images"
  def imagesDirectory: File = sourceDirectory(imagesDirectoryName)

  // src/main/fop/
  private def fopConfigurationDirectoryName: String = "fop"
  def fopConfigurationDirectory: File = sourceDirectory(fopConfigurationDirectoryName)
  private def fopConfigurationFileName: String = "fop.xconf"
  def fopConfigurationFile: File = new File(fopConfigurationDirectory, fopConfigurationFileName)

  // src/main/xsl/
  def customStylesheet(section: Section): String = section.name + "-custom.xsl"
  def paramsStylesheet(section: Section): String = section.name + "-params.xsl"
  private def stylesheetDirectoryName: String = "xsl"
  def stylesheetDirectory: File = sourceDirectory(stylesheetDirectoryName)
  def stylesheetFile(name: String) = new File(stylesheetDirectory, name)

  // src/main/xml/
  def catalogGroupBase: String = "../../.."  // to get to the project root // TODO file separator
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
  def buildDirectoryRelative(name: String): String = s"$buildDirRelative/$name/"  // TODO file separator!

  // Node
  def nodeRoot: File = /*cacheDirectory*/ buildDirectory("nodejs")
  def nodeModulesRoot: File = buildDir

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

  def forDocument(prefixed: Boolean, documentName: String): Layout.ForDocument = new Layout.ForDocument {

    override def mainStylesheet(section: Section): String =
      section.name + (if (!prefixed) "" else "-" + documentName) + ".xsl"

    override def baseDir(docBook2: DocBook2): String =
      if (!useRelativeBaseDir) saxonOutputDirectory(docBook2).getAbsolutePath + "/" else { // TODO file separator
        val directoryName: String = if (docBook2.usesIntermediate) intermediateRootName else outputRootName
        val directoryNameEffective: String = if (!prefixed) directoryName else s"$directoryName/$documentName" // TODO file separator
        buildDirectoryRelative(s"$directoryNameEffective/${saxonOutputDirectoryName(docBook2)}") // TODO file separator
      }

    override def saxonOutputDirectory(docBook2: DocBook2): File = outputDirectory(
      root = if (docBook2.usesIntermediate) intermediateRoot else outputRoot,
      subdirectoryName = saxonOutputDirectoryName(docBook2)
    )

    override def saxonOutputFile(docBook2: DocBook2): File = outputFile(
      docBook2,
      saxonOutputDirectory(docBook2),
      if (docBook2.usesIntermediate) docBook2.intermediateFileExtension else docBook2.outputFileExtension
    )

    override def outputDirectory(docBook2: DocBook2): File = outputDirectory(
      root = outputRoot,
      subdirectoryName = docBook2.name
    )

    override def outputFile(docBook2: DocBook2): File = outputFile(
      docBook2,
      outputDirectory(docBook2),
      docBook2.outputFileExtension
    )

    private def outputDirectory(
      root: File,
      subdirectoryName: String
    ): File = new File(
      if (!prefixed) root else new File(root, documentName),
      subdirectoryName
    )

    private def outputFile(
      docBook2: DocBook2,
      directory: File,
      extension: String
    ): File =
      new File(directory, docBook2.rootFilename(documentName) + "." + extension)
  }
}

object Layout {
  trait ForDocument {
    def mainStylesheet(section: Section): String

    def baseDir(docBook2: DocBook2): String

    def saxonOutputDirectory(docBook2: DocBook2): File

    def saxonOutputFile(docBook2: DocBook2): File

    def outputDirectory(docBook2: DocBook2): File

    def outputFile(docBook2: DocBook2): File
  }

  def forProject(project: Project): Layout =
    new Layout(project.getProjectDir, project.getBuildDir)

  def forRoot(root: File): Layout =
    new Layout(root, new File(root, "build"))
}
