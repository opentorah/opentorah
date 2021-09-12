package org.opentorah.docbook.section

object Epub3 extends Epub:
  override def name: String = "epub3"
  override protected def stylesheetUriName: String = "epub3/chunk"
  // DocBook XSLT stylesheets for EPUB3 use chunkfast (for xhtml) internally already :)
