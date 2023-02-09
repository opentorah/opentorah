package org.opentorah.docbook

import org.opentorah.xml.ScalaXml

object Pdf extends XsltFormat:
  override def name: String = "pdf"
  override protected def stylesheetUriName: String = "fo/docbook"
  override def outputFileExtension: String = "pdf"
  override def usesRootFile: Boolean = true
  override def usesIntermediate: Boolean = true
  override def intermediateFileExtension: String = "fo"
  override def common: List[Common] = List(AllCommon)

  override def parameters: Map[String, String] = Map(
    // Paper size; double-sidedness; not a draft
    "paper.type" -> "USletter",
    "double.sided" -> "yes",
    "draft.mode" -> "no",

    // FOP extensions
    "fop.extensions" -> "0",
    "fop1.extensions" -> "1"
  )

  override def calculatedParameters(values: CalculatedParameters): Map[String, String] = Map.empty

  override def usesCss: Boolean = false

  override protected def mainStylesheetCalculated(values: CalculatedParameters): ScalaXml.Nodes = Seq.empty

  override def customStylesheetBody: ScalaXml.Nodes = Seq(
    <!-- Break before each section -->,
    <xsl:attribute-set name="section.title.level1.properties">
      <xsl:attribute name="break-before">page</xsl:attribute>
    </xsl:attribute-set>
  )
