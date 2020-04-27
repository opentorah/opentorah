package org.opentorah.docbook.section

import java.io.File

trait Epub extends DocBook2 {
  final override def outputFileExtension: String = "epub"
  final override def usesRootFile: Boolean = false
  final override def copyDestinationDirectoryName: Option[String] = Some("OEBPS")
  final override def usesIntermediate: Boolean = true
  final override def additionalSections: List[Section] =  List(HtmlCommon, Common)
  final override def epubEmbeddedFontsParameter: Option[String] = Some("epub.embedded.fonts")
  final override def baseDirParameter: Option[String] = Some("base.dir")
  final override def rootFilenameParameter: Option[String] = Some("root.filename")
  final override def chunkQuietlyParameter: Option[String] = Some("chunk.quietly")

  final override def defaultParameters: Map[String, String] = Map(
  )

  final override def customStylesheet: String = ""

  final override def postProcess(
    inputDirectory: File,
    outputFile: File
  ): Unit = {
    val zip = new org.apache.tools.ant.taskdefs.Zip
    zip.setProject(new org.apache.tools.ant.Project)
    zip.setPreserve0Permissions(true)
    zip.setCompress(false)
    zip.setDestFile(outputFile)
    val fileSet = new org.apache.tools.ant.types.FileSet()
    fileSet.setDir(inputDirectory)
    fileSet.appendIncludes(Array("mimetype", "META-INF/**", "OEBPS/**"))
    zip.addFileset(fileSet)
    zip.execute()
  }
}
