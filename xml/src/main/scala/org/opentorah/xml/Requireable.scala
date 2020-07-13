package org.opentorah.xml

trait Requireable[T] {

  def optional: Parser[Option[T]]

  final def required: Parser[T] = for {
    result <- optional
    _ <- Parser.check(result.isDefined, s"Required $this is missing")
  } yield result.get
}
