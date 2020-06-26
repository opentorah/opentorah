package org.opentorah.docbook.section

object HtmlCommon extends CommonSection {
  override def name: String = "htmlCommon"

  override def parameters(isInfoEnabled: Boolean): Section.Parameters = Map(
    "use.id.as.filename" -> "yes",
    "chunker.output.encoding" -> "UTF-8",
    "chunker.output.indent" -> "yes",
    "chunk.first.sections" -> "1",
    "chunk.quietly" -> (if (isInfoEnabled) "0" else "1")
  )

  override protected def customStylesheetBody: String = ""
}
