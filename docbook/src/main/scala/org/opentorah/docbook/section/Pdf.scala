package org.opentorah.docbook.section

object Pdf extends DocBook2 {
  override def name: String = "pdf"
  override def isPdf: Boolean = true
  override def stylesheetUriName: String = "fo/docbook"
  override def outputFileExtension: String = "pdf"
  override def usesRootFile: Boolean = true
  override def usesIntermediate: Boolean = true
  override def intermediateDirectoryName: String = "fo"
  override def intermediateFileExtension: String = "fo"
  override def additionalSections: List[Section] = List( Common)

  override def defaultParameters: Map[String, String] = Map(
    // Paper size; double-sidedness; not a draft
    "paper.type" -> "USletter",
    "double.sided" -> "yes",
    "draft.mode" -> "no",

    // FOP extensions
    "fop.extensions" -> "0",
    "fop1.extensions" -> "1"
  )

  override def mainStylesheet(isMathJaxEnabled: Boolean): String = ""

  override def customStylesheet: String =
    s"""
       |  <!-- Break before each section -->
       |  <xsl:attribute-set name="section.title.level1.properties">
       |    <xsl:attribute name="break-before">page</xsl:attribute>
       |  </xsl:attribute-set>
       |""".stripMargin
}
