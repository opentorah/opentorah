package org.opentorah.xml

trait Parsable[T] {
  protected def parser: Parser[T]

  final def apply(): Parser[T] = parser

  def unparser: Unparser[T]

  final def apply[B](f: B => T): Unparser[B] = Unparser[B](
    namespace  = unparser.namespace,
    attributes = unparser.attributes compose f,
    content    = unparser.content compose f
  )
}
