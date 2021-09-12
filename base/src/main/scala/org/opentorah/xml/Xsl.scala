package org.opentorah.xml

object Xsl extends Dialect:

  override val namespace: Namespace = Namespace(uri = "http://www.w3.org/1999/XSL/Transform", prefix = "xsl")

  override val mimeType: String =  "application/xslt+xml"

  def version(usesDocBookXslt2: Boolean): String = if usesDocBookXslt2 then "2.0" else "1.0"
