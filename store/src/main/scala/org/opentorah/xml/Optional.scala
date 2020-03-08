package org.opentorah.xml

trait Optional[A] {

  def optional: Parser[Option[A]]

  final def required: Parser[A] = for {
    result <- optional
    _ <- Parser.check(result.isDefined, s"Required $this is missing")
  } yield result.get

}
