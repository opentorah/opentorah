package org.podval.docbook.gradle

import java.io.File
import org.gradle.api.Project

object Locations {
  // If used in DocBook files, this prefix points to the data directory
  val docBookDataUrl: String = "http://podval.org/docbook/data/"

  // If used in DocBook files, those point to the DocBook XSL files.
  val docBookXslUrl: String = "http://podval.org/docbook/xsl/"
  val docBookXslUrlOfficial: String = "http://docbook.sourceforge.net/release/xsl-ns/current/"

  def explodeDocBookXslInto(project: Project): File = buildDirectory(project, "docBookXsl")
  def docBookXsl(project: Project): File = new File(explodeDocBookXslInto(project), "docbook")

  def xslDir(project: Project): File = projectDirectory(project, "xsl")
  def xslFile(project: Project, name: String): File = file(xslDir(project), name, "xsl")

  def docBookDir(project: Project): File = projectDirectory(project, "docbook")

  def fopConfiguration(project: Project): File = projectDirectory(project, "fop/fop.xconf")

  def dataDirectory(project: Project): File = buildDirectory(project, "data")

  def imagesDirectory(project: Project): File = projectDirectory(project, "images")

  def outputDirectory(project: Project, name: String): File = buildDirectory(project, name)

  private  def projectDirectory(project: Project, name: String): File = new File(srcMain(project), name)

  def buildDirectory(project: Project, name: String): File = new File(project.getBuildDir, name)

  // Should get the main sourceSet, but this is only available via JavaConventions...
  private def srcMain(project: Project): File = new File(project.getProjectDir, "src/main")

  def file(directory: File, name: String, extension: String): File = new File(directory, name + "." + extension)
}
