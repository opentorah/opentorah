package org.opentorah.docbook.section

import Section.Parameters
import org.opentorah.xml.Namespace
import scala.xml.{Elem, Node}

trait Section {

  def name: String

  def parameters: Parameters

  def nonOverridableParameters(values: NonOverridableParameters): Parameters

  final def customStylesheet: Elem =
    <xsl:stylesheet xmlns:xsl={Namespace.Xsl.uri} version={Namespace.Xsl.version(usesDocBookXslt2)}
      xmlns:db={Namespace.DocBook.uri}
      exclude-result-prefixes="db">
      <!-- Customizations go here. -->
      {customStylesheetBody}
    </xsl:stylesheet>

  protected def customStylesheetBody: Seq[Node]

  def usesDocBookXslt2: Boolean = false
}

object Section {
  type Parameters = Map[String, String]

  val all: List[Section] = CommonSection.all ++ DocBook2.all
}
