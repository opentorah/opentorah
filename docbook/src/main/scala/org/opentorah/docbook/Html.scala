package org.opentorah.docbook

import org.opentorah.xml.ScalaXml

object Html extends XsltFormat:
  override def name: String = "html"
  override protected def stylesheetUriName: String = "html/chunkfast"
  override def outputFileExtension: String = "html"
  override protected def outputFileNameOverride: Option[String] = Some("index")
  override def usesRootFile: Boolean = false
  override def common: List[Common] = List(AllCommon, HtmlCommon)
  override def parameters: Map[String, String] = Map.empty

  override def calculatedParameters(values: CalculatedParameters): Map[String, String] = Map(
    values.rootFilename,
    values.htmlStylesheet
  ) ++
    values.mathJaxConfigurationParameter

  override def usesCss: Boolean = true

  override protected def mainStylesheetCalculated(values: CalculatedParameters): ScalaXml.Nodes =
    values.mathJaxUserHeadContent

  override def customStylesheetBody: ScalaXml.Nodes = Seq.empty
