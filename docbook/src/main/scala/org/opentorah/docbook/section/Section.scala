package org.opentorah.docbook.section

import org.opentorah.docbook.plugin.DocBook
import org.opentorah.xml.{Namespace, Xml}
import Section.Parameters

trait Section {

  def name: String

  def parameters: Parameters

  def nonOverridableParameters(values: NonOverridableParameters): Parameters

  final def customStylesheet: String =
    s"""${Xml.header}
       |<!-- Customizations go here. -->
       |<xsl:stylesheet $xslWithVersion
       |  xmlns:db="${DocBook.Namespace.uri}"
       |  exclude-result-prefixes="db">
       |
       |$customStylesheetBody
       |</xsl:stylesheet>
       |""".stripMargin

  protected def customStylesheetBody: String

  protected final def xslWithVersion: String =
    Namespace.Xsl.withVersion(if (usesDocBookXslt2) "2.0" else "1.0")

  def usesDocBookXslt2: Boolean = false
}

object Section {
  type Parameters = Map[String, String]

  val all: List[Section] = CommonSection.all ++ DocBook2.all
}
