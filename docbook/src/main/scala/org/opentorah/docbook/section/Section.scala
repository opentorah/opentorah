package org.opentorah.docbook.section

import Section.Parameters
import org.opentorah.docbook.DocBook
import org.opentorah.xml.{ScalaXml, Xsl}

trait Section derives CanEqual:

  def name: String

  final override def equals(other: Any): Boolean = this.name == other.asInstanceOf[Section].name

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

object Section:
  type Parameters = Map[String, String]

  val all: List[Section] = CommonSection.all ++ DocBook2.all
