package org.opentorah.docbook

import org.gradle.api.Project
import org.opentorah.fop.Fop
import org.opentorah.util.Files
import org.opentorah.xml.PrettyPrinter
import java.io.File

trait Layout:
  def root: File
  def src: File = Files.file(root, "src", "main")
  final def input: File = Files.file(src, "docBook")
  def build: File = Files.file(root, "build", "docBook")
  final def tmp: File = Files.file(build, "tmp")
  final def out: File = Files.file(build, Layout.outDirectory)

  def documentTmp(documentName: String): File = Files.file(tmp, documentName)
  def dtd(documentName: String): File = Files.file(documentTmp(documentName), "substitutions.dtd")
  def data(documentName: String): File = Files.file(documentTmp(documentName), "data")

  final def customCatalog: File = Files.file(src, Layout.catalogDirectory, "catalog-custom.xml")

  final def customStylesheetSrc(name: String): File = Files.file(src, Layout.xslDirectory, name + "-custom.xsl")
  final def customStylesheetTmp(name: String): File = Files.file(tmp, Layout.xslDirectory, name + "-custom.xsl")

  final def fopConfigurationFile: File =
    val srcFile: File = Files.fileSeq(src, Layout.fopConfiguration)
    if srcFile.exists then srcFile else
      val tmpFile: File = Files.fileSeq(tmp, Layout.fopConfiguration)
      if !tmpFile.exists then PrettyPrinter.default.write(
        file = tmpFile,
        replace = false,
        element = Fop.configurationFileDefault
      )
      tmpFile

object Layout:
  val fopConfiguration: Seq[String] = Seq("fop", "fop.xconf")
  val outDirectory    : String = "out"
  val catalogDirectory: String = "xml"
  val xslDirectory    : String = "xsl"
  val cssDirectory    : String = "css"
  val cssFile         : String = "docBook.css"
  val imagesDirectory : String = "images"

  def forRoot(directory: File): Layout = new Layout:
    override def root: File = directory

  def forGradleProject(project: Project): Layout = new Layout:
    override def root: File = project.getProjectDir
    // TODO configure src from the project
    override def build: File = Files.file(project.getBuildDir, "docBook")
