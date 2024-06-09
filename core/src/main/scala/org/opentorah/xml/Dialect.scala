package org.opentorah.xml

trait Dialect:
  def namespace: Namespace

  def mimeType: String

  def prettyPrinter: PrettyPrinter

  def rootElementName: String
  
  def dtdId: Option[String] = None

  def dtdUri: Option[String] = None
  
  final def doctype: String = doctype(rootElementName)
  
  final def doctype(rootElementName: String): String =
    val ids: String = dtdId.fold("")(it => s"""" $it"""") + dtdUri.fold("")(it => s"""" $it"""")
    val inner: String = if ids.isEmpty then "" else s" PUBLIC$ids"
    s"<!DOCTYPE $rootElementName$inner>"
