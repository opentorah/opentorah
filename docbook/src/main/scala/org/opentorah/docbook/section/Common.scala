package org.opentorah.docbook.section

import Section.Parameters
import org.opentorah.xml.ScalaXml

object Common extends CommonSection {
  override def name: String = "common"

  override def parameters: Parameters = Map(
    "toc.section.depth" -> "4",
    "section.autolabel" -> "0",
    "runinhead.default.title.end.punct" -> ""
  )

  override def nonOverridableParameters(values: NonOverridableParameters): Parameters = Map(
    "img.src.path" -> (values.imagesDirectoryName + "/")
  )

  override protected def customStylesheetBody: ScalaXml.Nodes = Seq(
    <!-- This is needed (?) for template-tweaking customizations, like removal of "Chapter" in chapter title -->,
    <xsl:param name="local.l10n.xml" select="document('')"/>,
    <!-- Remove "Chapter" in chapter title -->,
    <l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0">
      <l:l10n language="en">
        <l:context name="title-numbered">
          <l:template name="chapter" text="%n.&#160;%t"/>
        </l:context>
      </l:l10n>
    </l:i18n>
  )
}
