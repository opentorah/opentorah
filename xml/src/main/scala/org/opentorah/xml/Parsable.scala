package org.opentorah.xml

import org.opentorah.util.Collections
import java.net.URL
import zio.ZIO

trait Parsable[A] extends Requireable[A] {

  def canParse(elementName: String): Option[ToParse[A]]

  def mustParse(elementName: String): ToParse[A] = canParse(elementName).get

  final override val optional: Parser[Option[A]] = for {
    // TODO take namespace into account!
    elementOpt <- Context.nextElement(element => canParse(element.label).isDefined)
    result <- elementOpt.fold[Parser[Option[A]]](ZIO.none)(element =>
      nested(None, element, mustParse(element.label)).map(Some(_)))
  } yield result

  final def all: Parser[Seq[A]] = all(Seq.empty)

  private def all(acc: Seq[A]): Parser[Seq[A]] = for {
    opt <- optional
    result <- opt.fold[Parser[Seq[A]]](ZIO.succeed(acc))(next => all(acc :+ next))
  } yield result

  final def parse(fromUrl: URL): Parser[A] = parse(From.url(fromUrl))

  final def parse(from: From): Parser[A] = for {
    _ <- Context.checkNoLeftovers
    nextElement <- from.load
    name = nextElement.label
    result <- canParse(name)
      .fold[Parser[A]](ZIO.fail(s"$this required, but '$name' found"))(toParse =>
        nested(Some(from), nextElement, toParse))
  } yield result

  private def nested(
    from: Option[From],
    nextElement: Xml.Element,
    toParse: ToParse[A]
  ): Parser[A] = for {
    newCurrent <- Current.open(from, nextElement, toParse.contentType)
    result <- Context.nested(newCurrent, toParse.parser)
  } yield result
}

object Parsable {

  private class Compound[A](elements: Seq[Element[A]]) extends Parsable[A] {
      Collections.checkNoDuplicates(elements.map(_.elementName), "Parsable.union(elements.map(_.elementName))")

      override def canParse(elementName: String): Option[ToParse[A]] =
        elements.find(_.elementName == elementName).map(_.mustParse(elementName))
    }

  // TODO upgrade callers to full Parsablw with ToXml and eliminate:
  final def unionWithoutToXml[A](elements: Seq[Element[A]]): Parsable[A] = new Compound[A](elements)

  final def union[A](valueElementName: A => String, elements: Seq[Element.WithToXml[A]]): Parsable[A] with ToXml[A] =
    new Compound[A](elements) with ToXml[A] {
      override def toXmlElement(value: A): Xml.Element =
        elements.find(_.elementName == valueElementName(value)).get.toXmlElement(value)
    }
}
