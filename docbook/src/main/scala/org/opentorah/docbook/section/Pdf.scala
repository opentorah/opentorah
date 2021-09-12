package org.opentorah.docbook.section

import Section.Parameters
import org.opentorah.xml.ScalaXml

object Pdf extends DocBook2:
  override def name: String = "pdf"
  override def isPdf: Boolean = true
  override protected def stylesheetUriName: String = "fo/docbook"
  override protected def outputFileExtension: String = "pdf"
  override def usesRootFile: Boolean = true
  override def usesIntermediate: Boolean = true
  override protected def intermediateFileExtension: String = "fo"
  override def commonSections: List[CommonSection] = List(Common)

  override def parameters: Parameters = Map(
    // Paper size; double-sidedness; not a draft
    "paper.type" -> "USletter",
    "double.sided" -> "yes",
    "draft.mode" -> "no",

    // FOP extensions
    "fop.extensions" -> "0",
    "fop1.extensions" -> "1"
  )

  override def nonOverridableParameters(values: NonOverridableParameters): Parameters = Map.empty

  override def usesCss: Boolean = false

  override protected def mainStylesheetBody(values: NonOverridableParameters): ScalaXml.Nodes = Seq.empty

  override protected def customStylesheetBody: ScalaXml.Nodes = Seq(
    <!-- Break before each section -->,
    <xsl:attribute-set name="section.title.level1.properties">
      <xsl:attribute name="break-before">page</xsl:attribute>
    </xsl:attribute-set>
  )
