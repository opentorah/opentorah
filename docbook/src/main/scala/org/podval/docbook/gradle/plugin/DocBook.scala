package org.podval.docbook.gradle.plugin

object DocBook {

  val dtdId: String = "-//OASIS//DTD DocBook XML V5.0//EN"

  val dtdUri: String = "http://www.oasis-open.org/docbook/xml/5.0/dtd/docbook.dtd"

  val doctype: String = s"""<!DOCTYPE article PUBLIC "$dtdId" "$dtdUri">"""

  object Namespace extends org.podval.fop.xml.Namespace(uri = "http://docbook.org/ns/docbook") {
    def withVersion: String = withVersion("5.0")
  }
}
