package org.opentorah.docbook.section

// MathJax 2
//<script async src="https://cdn.jsdelivr.net/npm/mathjax@2/MathJax.js?config=TeX-AMS-MML_CHTML"></script>

// MathJax 3
//<script src="https://polyfill.io/v3/polyfill.min.js?features=es6"></script>
//<script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
//or
//<script id="MathJax-script" defer src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js" type="text/javascript"></script>

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
  override def mathJaxConfigurationParameter: Option[String] = Some(mathJaxConfigurationParameterName)

  override def defaultParameters: Map[String, String] = Map(
  )

  val mathJaxConfigurationParameterName: String = "mathjax.configuration"

  override def mainStylesheet(isMathJaxEnabled: Boolean): String = if (!isMathJaxEnabled) "" else
    s"""
       |  <!-- Add MathJax support -->
       |  <xsl:template name="user.head.content">
       |    <script type="text/javascript">
       |      window.MathJax = <xsl:value-of select = "$$$mathJaxConfigurationParameterName" />;
       |    </script>
       |    <script type="text/javascript" src="https://cdn.jsdelivr.net/npm/mathjax@2/MathJax.js?config=MML_HTMLorMML"/>
       |  </xsl:template>
       |""".stripMargin

  override def customStylesheet: String = ""
}
