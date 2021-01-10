package org.opentorah.xml

trait Parsable[T] {
  protected def parser: Parser[T]

  final def apply(): Parser[T] = parser

  def antiparser: Antiparser[T]

  final def apply[B](f: B => T): Antiparser[B] = Antiparser[B](
    antiparser.attributes compose f,
    antiparser.content compose f,
    antiparser.namespace
  )
}
