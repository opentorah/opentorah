package org.opentorah.docbook.section

import Section.Parameters
import org.opentorah.xml.Xml
import java.io.File

trait Epub extends DocBook2 {
  final override protected def outputFileExtension: String = "epub"
  final override def usesRootFile: Boolean = false
  final override def copyDestinationDirectoryName: Option[String] = Some("OEBPS")
  final override def usesIntermediate: Boolean = true
  final override def commonSections: List[CommonSection] =  List(Common, HtmlCommon)

  final override def parameters: Parameters = Map.empty

  final override def nonOverridableParameters(values: NonOverridableParameters): Parameters = Map(
    "root.filename" -> rootFilename(values.documentName),
    "epub.embedded.fonts" -> values.embeddedFonts
  )

  override def usesCss: Boolean = false

  final override protected def mainStylesheetBody(values: NonOverridableParameters): Xml.Nodes = Seq.empty

  final override protected def customStylesheetBody: Xml.Nodes = Seq.empty

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
