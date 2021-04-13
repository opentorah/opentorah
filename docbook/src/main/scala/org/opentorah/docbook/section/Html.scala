package org.opentorah.docbook.section

import Section.Parameters
import org.opentorah.mathjax.MathJax
import org.opentorah.xml.Xml

object Html extends DocBook2 {
  override def name: String = "html"
  override protected def stylesheetUriName: String = "html/chunkfast"
  override protected def outputFileExtension: String = "html"
  override protected def outputFileNameOverride: Option[String] = Some("index")
  override def usesRootFile: Boolean = false
  override def commonSections: List[CommonSection] = List(Common, HtmlCommon)

  override def parameters: Parameters = Map.empty

  override def nonOverridableParameters(values: NonOverridableParameters): Parameters = Map(
    "root.filename" -> rootFilename(values.documentName),
    "html.stylesheet" -> values.cssFile
  ) ++ values.mathJaxConfiguration.fold[Parameters](Map.empty){ mathJaxConfiguration =>
    val mathJax: MathJax = MathJax.get(values.useMathJax3)
    Map(
      mathJaxConfigurationParameterName -> mathJax.htmlConfigurationString(mathJaxConfiguration)
    )
  }

  override def usesCss: Boolean = true

  val mathJaxConfigurationParameterName: String = "mathjax.configuration"

  override protected def mainStylesheetBody(values: NonOverridableParameters): Xml.Nodes =
    if (values.mathJaxConfiguration.isEmpty) Seq.empty else {
      val mathJax: MathJax = MathJax.get(values.useMathJax3)
      Seq(
        <!-- Add MathJax support -->,
        <xsl:template name="user.head.content">
          {mathJax.head(<xsl:value-of select={s"$$$mathJaxConfigurationParameterName"}/>)}
        </xsl:template>
      )
    }

  override protected def customStylesheetBody: Xml.Nodes = Seq.empty
}
