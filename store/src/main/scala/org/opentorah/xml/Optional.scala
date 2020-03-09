package org.opentorah.xml

// TODO make AttributeLike Repeatable; collapse Repeatable into Optional;
// rename it Parsable; move named elements from Text to Element?
trait Optional[A] {

  def optional: Parser[Option[A]]

  final def required: Parser[A] = for {
    result <- optional
    _ <- Parser.check(result.isDefined, s"Required $this is missing")
  } yield result.get
}
