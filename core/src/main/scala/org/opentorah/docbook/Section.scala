package org.opentorah.docbook

import org.opentorah.xml.{ScalaXml, Xsl}

trait Section:

  def parameters: Map[String, String]

  def calculatedParameters(values: CalculatedParameters): Map[String, String]

  def customStylesheetBody: ScalaXml.Nodes
