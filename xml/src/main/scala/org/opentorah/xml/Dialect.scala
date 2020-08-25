package org.opentorah.xml

// TODO generalize into a Doctype trait?
trait Dialect {
  def namespace: Namespace

  def mimeType: String

  def prettyPrinter: PrettyPrinter = PrettyPrinter.default
}
