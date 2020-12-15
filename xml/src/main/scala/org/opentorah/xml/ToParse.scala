package org.opentorah.xml

trait ToParse[+A] {

  def contentType: ContentType

  def parser: Parser[A]

  // TODO move into Element and process
  def canDelegate: Boolean = false
}
