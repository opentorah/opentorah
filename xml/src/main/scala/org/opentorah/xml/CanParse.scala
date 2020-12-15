package org.opentorah.xml

final class CanParse[+A](
  contentType: ContentType,
  parser: Parser[A]
) {
  def nested(
    from: Option[From],
    nextElement: Xml.Element
  ): Parser[A] = Context.nested(
    from,
    nextElement,
    contentType,
    parser
  )

  // TODO clean up/abstract/eliminate
  def mapParser[B](f: Parser[A] => Parser[B]): CanParse[B] = new CanParse[B](
    contentType,
    f(parser)
  )
}
