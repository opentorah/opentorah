package org.podval.docbook.gradle.section

object Html extends DocBook2 {
  override def name: String = "html"
  override def stylesheetUriName: String = "html/chunkfast"
  override def outputFileExtension: String = "html"
  override def outputFileNameOverride: Option[String] = Some("index")
  override def usesRootFile: Boolean = false
  override def additionalSections: List[Section] = List(HtmlCommon, Common)
  override def baseDirParameter: Option[String] = Some("base.dir")
  override def rootFilenameParameter: Option[String] = Some("root.filename")
  override def htmlStylesheetsParameter: Option[String] = Some("html.stylesheet")
  override def chunkQuietlyParameter: Option[String] = Some("chunk.quietly")

  override def defaultParameters: Map[String, String] = Map(
  )

  override def customStylesheet: String =
    s"""
       |  <!-- Add MathJax support -->
       |  <xsl:template name="user.head.content">
       |    <script type="text/javascript">
       |      window.MathJax = { MathML: { extensions: [ "content-mathml.js", "mml3.js" ] } };
       |    </script>
       |    <script type="text/javascript" src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_HTMLorMML"/>
       |  </xsl:template>
       |"""
}
