package org.opentorah.xml

trait Dialect:
  def namespace: Namespace

  def mimeType: String

  def prettyPrinter: PrettyPrinter
