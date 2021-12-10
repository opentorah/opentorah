package org.opentorah.docbook

import org.opentorah.xml.ScalaXml

object Html2 extends XsltFormat:
  override def name: String = "html2"
  override protected def stylesheetUriName: String = "html/chunk"
  override def outputFileExtension: String = "html"
  override protected def outputFileNameOverride: Option[String] = Some("index")
  override def usesRootFile: Boolean = true
  override def usesDocBookXslt2: Boolean = true
  override def common: List[Common] = List.empty

  override def parameters: Map[String, String] = Map(
    "use.id.as.filename" -> "yes",
    "toc.section.depth" -> "4",
  )

  override def calculatedParameters(values: CalculatedParameters): Map[String, String] = Map(
    values.baseDir,
    values.htmlStylesheet
  )

  override def usesCss: Boolean = true

  override protected def mainStylesheetCalculated(values: CalculatedParameters): ScalaXml.Nodes = Seq.empty

  override def customStylesheetBody: ScalaXml.Nodes = Seq(
    <xsl:param name="autolabel.elements">
      <db:appendix format="A"/>
      <db:chapter/>
      <db:figure/>
      <db:example/>
      <db:table/>
      <db:equation/>
      <db:part format="I"/>
      <db:reference format="I"/>
      <db:preface/>
      <db:qandadiv/>
      <db:section/>
      <db:refsection/>
    </xsl:param>
  )
