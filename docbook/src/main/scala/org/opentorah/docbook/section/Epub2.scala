package org.opentorah.docbook.section

object Epub2 extends Epub {
  override def name: String = "epub2"
  override def stylesheetUriName: String = "epub/docbook"
  // DocBook XSLT stylesheets for EPUB2 do not add the mimetype file with "application/epub+zip" in it!
}
