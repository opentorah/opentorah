package org.podval.docbook.gradle

import java.io.File

import org.gradle.api.Project

class Layout(projectDir: File, buildDir: File) {

  private def buildDirRelative: String =
    if (buildDir.getParentFile == projectDir) buildDir.getName
    else throw new IllegalArgumentException("buildDir that is not a child of projectDir is not yet supported! ")

  private def sourceDir: File = new File(projectDir, "src/main")
  private def sourceDirectory(name: String): File = new File(sourceDir, name)

  private def inputDirectoryName: String = "docBook"
  final def inputDirectory: File = sourceDirectory(inputDirectoryName)
  final def inputFile(inputFileName: String): File = new File(inputDirectory, inputFileName + ".xml")

  final def cssDirectoryName: String = "css"
  final def cssDirectory: File = sourceDirectory(cssDirectoryName)
  final def cssFileName: String = "docBook.css"
  final def cssFile: File = new File(cssDirectory, cssFileName)

  final def imagesDirectoryName: String = "images"
  final def imagesDirectory: File = sourceDirectory(imagesDirectoryName)

  private def fopConfigurationDirectoryName: String = "fop"
  final def fopConfigurationDirectory: File = sourceDirectory(fopConfigurationDirectoryName)

  private def fopConfigurationFileName: String = "fop.xconf"
  final def fopConfigurationFile: File = new File(fopConfigurationDirectory, fopConfigurationFileName)

  private def buildDirectory(name: String): File = new File(buildDir, name)
  private def buildDirectoryRelative(name: String): String = s"$buildDirRelative/$name/"

  private def dataDirectoryName: String = "data"
  final def dataDirectory: File = buildDirectory(dataDirectoryName)
  final def dataDirectoryRelative: String = buildDirectoryRelative(dataDirectoryName)

  private def substitutionsDtdFileName: String = "substitutions.dtd"
  final def substitutionsDtdFile: File = new File(buildDir, substitutionsDtdFileName)
  final def substitutionsDtdRelative: String = s"$buildDirRelative/$substitutionsDtdFileName"

  private def catalogFileName: String = "catalog.xml"
  final def catalogFile: File = new File(sourceDirectory("xml"), catalogFileName)


  trait ForXsltBase extends ForXslt {

    final override def stylesheetDirectory: File = sourceDirectory(stylesheetDirectoryName)
    final override def stylesheetFile(name: String) = new File(stylesheetDirectory, name + ".xsl")

    protected def docBookXslDirectoryName: String
    final override def docBookXslDirectory: File = buildDirectory(docBookXslDirectoryName)
    final def docBookXslDirectoryRelative: String = buildDirectoryRelative(docBookXslDirectoryName)

    protected def outputDirectoryName: String

    final override def outputDirectoryRoot: File = buildDirectory(outputDirectoryName)

    protected def saxonOutputDirectoryName: String

    final override def saxonOutputDirectoryRoot: File = buildDirectory(saxonOutputDirectoryName)
  }

  val forXslt1: ForXsltBase = new ForXsltBase {
    override def docBookXslConfigurationName: String = "docBookXsl"
    override def stylesheetDirectoryName: String = "xsl"
    protected override def docBookXslDirectoryName: String = "docBookXsl"
    override def docBookXslArchiveSubdirectoryName: String = "docbook"
    protected override def outputDirectoryName: String = "docBook"
    protected override def saxonOutputDirectoryName: String = "docBookTmp"
  }

  val forXslt2: ForXsltBase = new ForXsltBase {
    override def docBookXslConfigurationName: String = "docBookXsl2"
    override def stylesheetDirectoryName: String = "xsl2"
    protected override def docBookXslDirectoryName: String = "docBookXsl2"
    override def docBookXslArchiveSubdirectoryName: String = "xslt/base"
    protected override def outputDirectoryName: String = "docBook2"
    protected override def saxonOutputDirectoryName: String = "docBookTmp2"
  }

  final def forXslt(useDocBookXslt2: Boolean): ForXsltBase = if (useDocBookXslt2) forXslt2 else forXslt1
}

object Layout {
  def forProject(project: Project): Layout =
    new Layout(project.getProjectDir, project.getBuildDir)

  def forRoot(root: File): Layout =
    new Layout(root, new File(root, "build"))
}
