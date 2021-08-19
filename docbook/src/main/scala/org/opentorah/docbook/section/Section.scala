package org.opentorah.docbook.section

import Section.Parameters
import org.opentorah.docbook.DocBook
import org.opentorah.xml.{ScalaXml, Xsl}

trait Section {

  def name: String

  def parameters: Parameters

  def nonOverridableParameters(values: NonOverridableParameters): Parameters

  final def customStylesheet: ScalaXml.Element =
    <xsl:stylesheet xmlns:xsl={Xsl.namespace.uri} version={Xsl.version(usesDocBookXslt2)}
      xmlns:db={DocBook.namespace.uri}
      exclude-result-prefixes="db">
      <!-- Customizations go here. -->
      {customStylesheetBody}
    </xsl:stylesheet>

  protected def customStylesheetBody: ScalaXml.Nodes

  def usesDocBookXslt2: Boolean = false
}

object Section {
  type Parameters = Map[String, String]

  val all: List[Section] = CommonSection.all ++ DocBook2.all
}
