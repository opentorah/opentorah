package org.opentorah.docbook.section

import Section.Parameters
import org.opentorah.util.Json
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
  ) ++ values.mathJaxConfiguration.fold[Parameters](Map.empty)(mathJaxConfiguration => Map(
     mathJaxConfigurationParameterName -> Json.fromMap(mathJaxConfiguration.toHtmlMap)
  ))

  override def usesCss: Boolean = true

  val mathJaxConfigurationParameterName: String = "mathjax.configuration"

  override protected def mainStylesheetBody(values: NonOverridableParameters): Seq[Xml.Node] =
    if (values.mathJaxConfiguration.isEmpty) Seq.empty else Seq(
      <!-- Add MathJax support -->,
      <xsl:template name="user.head.content">
        <script type="text/javascript">
          window.MathJax=<xsl:value-of select={s"$$$mathJaxConfigurationParameterName"}/>;
        </script>
        <script type="text/javascript" src="https://cdn.jsdelivr.net/npm/mathjax@2/MathJax.js?config=MML_HTMLorMML"/>
      </xsl:template>
    )

  override protected def customStylesheetBody: Seq[Xml.Node] = Seq.empty
}
