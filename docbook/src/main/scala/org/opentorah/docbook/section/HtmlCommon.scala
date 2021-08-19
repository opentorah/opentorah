package org.opentorah.docbook.section

import Section.Parameters
import org.opentorah.xml.ScalaXml

object HtmlCommon extends CommonSection {
  override def name: String = "htmlCommon"

  override def parameters: Parameters = Map(
    "use.id.as.filename" -> "yes",
    "chunker.output.encoding" -> "UTF-8",
    "chunker.output.indent" -> "yes",
    "chunk.first.sections" -> "1"
  )

  override def nonOverridableParameters(values: NonOverridableParameters): Parameters = Map(
    "base.dir" -> (values.saxonOutputDirectory.getAbsolutePath + "/"),
    "chunk.quietly" -> (if (values.isInfoEnabled) "0" else "1")
  )

  override protected def customStylesheetBody: ScalaXml.Nodes = Seq.empty
}
