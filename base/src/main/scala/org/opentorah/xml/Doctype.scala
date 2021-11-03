package org.opentorah.xml

trait Doctype:
  def doctype: String

object Doctype:
  def string(rootElementName: String) =
    s"<!DOCTYPE $rootElementName>"

  def string(rootElementName: String, dtdId: String, dtdUri: String): String =
    s"""<!DOCTYPE catalog PUBLIC "$dtdId" "$dtdUri">"""