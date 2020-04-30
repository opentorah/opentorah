package org.opentorah.docbook.section

object Common extends Section {
  override def name: String = "common"

  override def defaultParameters: Map[String, String] = Map(
    "toc.section.depth" -> "4",
    "section.autolabel" -> "0",
    "runinhead.default.title.end.punct" -> ""
  )

  override def customStylesheet: String =
    s"""  <!-- This is needed (?) for template-tweaking customizations, like removal of "Chapter" in chapter title -->
       |  <xsl:param name="local.l10n.xml" select="document('')"/>
       |
       |  <!-- Remove "Chapter" in chapter title -->
       |  <l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0">
       |    <l:l10n language="en">
       |      <l:context name="title-numbered">
       |        <l:template name="chapter" text="%n.&#160;%t"/>
       |      </l:context>
       |    </l:l10n>
       |  </l:i18n>
       |""".stripMargin
}
