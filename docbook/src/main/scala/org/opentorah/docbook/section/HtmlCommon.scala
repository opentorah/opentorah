package org.opentorah.docbook.section

object HtmlCommon extends Section {
  override def name: String = "htmlCommon"

  override def defaultParameters: Map[String, String] = Map(
    "use.id.as.filename" -> "yes",
    "chunker.output.encoding" -> "UTF-8",
    "chunker.output.indent" -> "yes",
    "chunk.first.sections" -> "1"
  )

  override def customStylesheet: String = ""
}
