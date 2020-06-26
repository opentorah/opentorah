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
  override protected def outputFileExtension: String = "html"
  override protected def outputFileNameOverride: Option[String] = Some("index")
  override def usesRootFile: Boolean = false
  override def commonSections: List[CommonSection] = List(Common, HtmlCommon)

  override protected def baseDirParameter: Option[String] = Some("base.dir")
  override protected def rootFilenameParameter: Option[String] = Some("root.filename")
  override protected def htmlStylesheetsParameter: Option[String] = Some("html.stylesheet")
  override protected def mathJaxConfigurationParameter: Option[String] = Some(mathJaxConfigurationParameterName)

  override def parameters(isInfoEnabled: Boolean): Section.Parameters = Map.empty

  val mathJaxConfigurationParameterName: String = "mathjax.configuration"

  override protected def mainStylesheetBody(isMathJaxEnabled: Boolean): String = if (!isMathJaxEnabled) "" else
    s"""
       |  <!-- Add MathJax support -->
       |  <xsl:template name="user.head.content">
       |    <script type="text/javascript">
       |      window.MathJax = <xsl:value-of select = "$$$mathJaxConfigurationParameterName" />;
       |    </script>
       |    <script type="text/javascript" src="https://cdn.jsdelivr.net/npm/mathjax@2/MathJax.js?config=MML_HTMLorMML"/>
       |  </xsl:template>
       |""".stripMargin

  override protected def customStylesheetBody: String = ""
}
