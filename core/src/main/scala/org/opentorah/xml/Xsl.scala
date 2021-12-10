package org.opentorah.xml

object Xsl extends Dialect:

  override val namespace: Namespace = Namespace(uri = "http://www.w3.org/1999/XSL/Transform", prefix = "xsl")

  override val mimeType: String =  "application/xslt+xml"

  override def prettyPrinter: PrettyPrinter = PrettyPrinter.default

  def version(usesDocBookXslt2: Boolean): String = if usesDocBookXslt2 then "2.0" else "1.0"

  def stylesheet(usesDocBookXslt2: Boolean, content: ScalaXml.Nodes): ScalaXml.Element =
    <xsl:stylesheet xmlns:xsl={namespace.uri} version={version(usesDocBookXslt2)}>{content}</xsl:stylesheet>

  def xslImport(href:String): ScalaXml.Element = <xsl:import href={href}/>
