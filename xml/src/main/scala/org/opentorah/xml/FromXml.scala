package org.opentorah.xml

import java.net.URL
import zio.ZIO

// TODO fold ToXml in here and rename Parsable again ;)
// - but first, figure out how to deal with mapParser and Element.WithToXmlFromUrl...
trait FromXml[A] extends Requireable[A] {

  private[xml] def canParse(elementName: String): Option[CanParse[A]]

  final def mustParse(elementName: String): CanParse[A] = canParse(elementName)
    .getOrElse(throw new IllegalArgumentException(s"Can't parse '$elementName'")) // TODO use ZIO.fail!

  // TODO abstract/eliminate
  final def mapParser[B](f: Parser[A] => Parser[B]): FromXml[B] = new FromXml[B] {
    final override def canParse(elementName: String): Option[CanParse[B]] =
      FromXml.this.canParse(elementName).map(_.mapParser(f))
  }

  final override val optional: Parser[Option[A]] = for {
    // TODO take namespace into account!
    elementOpt <- Context.nextElement(element => canParse(element.label).isDefined)
    result <- elementOpt
      .map(element => mustParse(element.label).nested(None, element).map(Some(_)))
      .getOrElse(ZIO.none)
  } yield result

  final def all: Parser[Seq[A]] = all(Seq.empty)

  private def all(acc: Seq[A]): Parser[Seq[A]] = for {
    opt <- optional
    result <- opt
      .map(next => all(acc :+ next))
      .getOrElse(ZIO.succeed(acc))
  } yield result

  final def parse(fromUrl: URL): Parser[A] = parse(From.url(fromUrl))

  final def parse(from: From): Parser[A] = for {
    _ <- Context.checkNoLeftovers
    nextElement <- from.load
    name = nextElement.label
    result <- canParse(name)
      .map(_.nested(Some(from), nextElement))
      .getOrElse(ZIO.fail(s"$this required, but '$name' found"))
  } yield result
}
