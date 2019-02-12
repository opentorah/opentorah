package org.podval.docbook.gradle

import java.io.File

import org.gradle.api.Project

class Layouts(projectDir: File, buildDir: File) {

  trait LayoutBase extends Layout {
    private def sourceDir: File = new File(projectDir, "src/main")
    private def sourceDirectory(name: String): File = new File(sourceDir, name)

    private def inputDirectoryName: String = "docBook"
    private def inputDirectory: File = sourceDirectory(inputDirectoryName)

    final override def inputFile(inputFileName: String): File = new File(inputDirectory, inputFileName + ".xml")

    private def stylesheetDirectory: File = sourceDirectory(stylesheetDirectoryName)
    final override def stylesheetFile(name: String) = new File(stylesheetDirectory, name + ".xsl")

    final override def imagesDirectoryName: String = "images"
    final override def imagesDirectory: File = sourceDirectory(imagesDirectoryName)

    final override def cssDirectoryName: String = "css"
    final override def cssDirectory: File = sourceDirectory(cssDirectoryName)
    final override def cssFileName: String = "docBook.css"
    final override def cssFile: File = new File(cssDirectory, cssFileName)

    private def fopConfigurationDirectoryName: String = "fop"
    private def fopConfigurationDirectory: File = sourceDirectory(fopConfigurationDirectoryName)
    private def fopConfigurationFileName: String = "fop.xconf"
    final override def fopConfigurationFile: File = new File(fopConfigurationDirectory, fopConfigurationFileName)

    private def buildDirectory(name: String): File = new File(buildDir, name)

    protected def docBookXslDirectoryName: String

    final override def docBookXslDirectory: File = buildDirectory(docBookXslDirectoryName)

    private def dataDirectoryName: String = "data"
    final override def dataDirectory: File = buildDirectory(dataDirectoryName)

    protected def outputDirectoryName: String

    final override def outputDirectoryRoot: File = buildDirectory(outputDirectoryName)

    protected def saxonOutputDirectoryName: String

    final override def saxonOutputDirectoryRoot: File = buildDirectory(saxonOutputDirectoryName)

    def inputDirectories: Set[File] = Set(
      inputDirectory,
      stylesheetDirectory,
      dataDirectory,
      imagesDirectory,
      cssDirectory,
      fopConfigurationDirectory
    )
  }

  val forXslt1: LayoutBase = new LayoutBase {
    override def useDocBookXslt2: Boolean = false
    override def docBookXslConfigurationName: String = "docBookXsl"
    override def stylesheetDirectoryName: String = "xsl"
    protected override def docBookXslDirectoryName: String = "docBookXsl"
    override def docBookXslArchiveSubdirectoryName: String = "docbook"
    protected override def outputDirectoryName: String = "docBook"
    protected override def saxonOutputDirectoryName: String = "docBookTmp"
  }

  val forXslt2: LayoutBase = new LayoutBase {
    override def useDocBookXslt2: Boolean = true
    override def docBookXslConfigurationName: String = "docBookXsl2"
    override def stylesheetDirectoryName: String = "xsl2"
    protected override def docBookXslDirectoryName: String = "docBookXsl2"
    override def docBookXslArchiveSubdirectoryName: String = "xslt/base"
    protected override def outputDirectoryName: String = "docBook2"
    protected override def saxonOutputDirectoryName: String = "docBookTmp2"
  }
}

object Layouts {
  def forProject(project: Project): Layouts =
    new Layouts(project.getProjectDir, project.getBuildDir)

  def forRoot(root: File): Layouts =
    new Layouts(root, new File(root, "build"))
}
