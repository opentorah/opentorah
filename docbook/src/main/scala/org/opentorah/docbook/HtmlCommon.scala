package org.opentorah.docbook

import org.opentorah.xml.ScalaXml

object HtmlCommon extends Common:
  override def name: String = "html"

  override def parameters: Map[String, String] = Map(
    "use.id.as.filename" -> "yes",
    "chunker.output.encoding" -> "UTF-8",
    "chunker.output.indent" -> "yes",
    "chunk.first.sections" -> "1"
  )

  override def calculatedParameters(values: CalculatedParameters): Map[String, String] = Map(
    values.baseDir,
    values.chunkQuietly
  )

  override def customStylesheetBody: ScalaXml.Nodes = Seq.empty
