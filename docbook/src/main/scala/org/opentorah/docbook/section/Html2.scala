package org.opentorah.docbook.section

object Html2 extends DocBook2 {
  override def name: String = "html2"
  override def stylesheetUriName: String = "html/chunk"
  override def outputFileExtension: String = "html"
  override protected def outputFileNameOverride: Option[String] = Some("index")
  override def usesRootFile: Boolean = true
  override def usesDocBookXslt2: Boolean = true
  override def commonSections: List[CommonSection] = List.empty

  override protected def baseDirParameter: Option[String] = Some("base.dir")
  override protected def htmlStylesheetsParameter: Option[String] = Some("html.stylesheets")

  override def defaultParameters: Section.Parameters = Map(
    "use.id.as.filename" -> "yes",
    "toc.section.depth" -> "4"
  )

  override protected def mainStylesheetBody(isMathJaxEnabled: Boolean): String = ""

  override protected def customStylesheetBody: String =
    s"""
       |  <xsl:param name="autolabel.elements">
       |    <db:appendix format="A"/>
       |    <db:chapter/>
       |    <db:figure/>
       |    <db:example/>
       |    <db:table/>
       |    <db:equation/>
       |    <db:part format="I"/>
       |    <db:reference format="I"/>
       |    <db:preface/>
       |    <db:qandadiv/>
       |    <db:section/>
       |    <db:refsection/>
       |  </xsl:param>
       |""".stripMargin
}
