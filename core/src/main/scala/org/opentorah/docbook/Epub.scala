package org.opentorah.docbook

import org.opentorah.util.Zip
import org.opentorah.xml.ScalaXml
import java.io.File

trait Epub extends XsltFormat:
  final override def outputFileExtension: String = "epub"
  final override def usesRootFile: Boolean = false
  final override def copyDestinationDirectoryName: Option[String] = Some("OEBPS")
  final override def usesIntermediate: Boolean = true
  final override def common: List[Common] = List(AllCommon, HtmlCommon)

  final override def parameters: Map[String, String] = Map.empty

  final override def calculatedParameters(values: CalculatedParameters): Map[String, String] = Map(
    values.rootFilename
  ) ++
    values.epubEmbeddedFonts

  override def usesCss: Boolean = false

  final override protected def mainStylesheetCalculated(values: CalculatedParameters): ScalaXml.Nodes = Seq.empty

  final override def customStylesheetBody: ScalaXml.Nodes = Seq.empty

  final override def postProcess(
    inputDirectory: File,
    outputFile: File
  ): Unit = Zip.zip(outputFile, Seq(
    File(inputDirectory, "mimetype"),
    File(inputDirectory, "META-INF"),
    File(inputDirectory, "OEBPS")
  ))
