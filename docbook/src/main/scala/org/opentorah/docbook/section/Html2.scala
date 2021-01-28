package org.opentorah.docbook.section

import Section.Parameters
import org.opentorah.xml.Xml

object Html2 extends DocBook2 {
  override def name: String = "html2"
  override protected def stylesheetUriName: String = "html/chunk"
  override protected def outputFileExtension: String = "html"
  override protected def outputFileNameOverride: Option[String] = Some("index")
  override def usesRootFile: Boolean = true
  override def usesDocBookXslt2: Boolean = true
  override def commonSections: List[CommonSection] = List.empty

  override def parameters: Parameters = Map(
    "use.id.as.filename" -> "yes",
    "toc.section.depth" -> "4"
  )

  override def nonOverridableParameters(values: NonOverridableParameters): Parameters = Map(
    "base.dir" -> (values.saxonOutputDirectory.getAbsolutePath + "/"),
    "html.stylesheet" -> values.cssFile
  )

  override def usesCss: Boolean = true

  override protected def mainStylesheetBody(values: NonOverridableParameters): Xml.Nodes = Seq.empty

  override protected def customStylesheetBody: Xml.Nodes = Seq(
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
}
