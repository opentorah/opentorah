package org.opentorah.docbook

import org.opentorah.xml.{Dialect, Namespace, PrettyPrinter}

object DocBook extends Dialect {

  override val namespace: Namespace = Namespace(uri = "http://docbook.org/ns/docbook", prefix="db")

  override val mimeType: String = "application/xml"

  val dtdId: String = "-//OASIS//DTD DocBook XML V5.0//EN"

  val dtdUri: String = "http://www.oasis-open.org/docbook/xml/5.0/dtd/docbook.dtd"

  val doctype: String = s"""<!DOCTYPE article PUBLIC "$dtdId" "$dtdUri">"""

  val version: String = "5.0"

  override val prettyPrinter: PrettyPrinter = PrettyPrinter(
    alwaysStackElements = Set("article", "para", "equation", "informalequation", "inlineequation", "math", "mrow", "mi")
  )
}
