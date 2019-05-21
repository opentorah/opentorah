package org.podval.docbook.gradle

import org.podval.docbook.gradle.xml.Namespace

object DocBook extends Namespace(uri = "http://docbook.org/ns/docbook") {
  def withVersion: String = toString + " version=\"5.0\""

  val dtdId: String = "-//OASIS//DTD DocBook XML V5.0//EN"

  val dtdUri: String = "http://www.oasis-open.org/docbook/xml/5.0/dtd/docbook.dtd"
}
