package org.digitaljudaica.xml

import scala.xml.Node

abstract class Descriptor[A](
  val elementName: String,
  val contentType: ContentType = ContentType.Elements,
  val contentParser: Parser[A]
) extends Parsable[A] with ToXml[A] {
  final override def required: Parser[A] =
    Element(elementName, contentType, contentParser).required

  final override def optional: Parser[Option[A]] =
    Element(elementName, contentType, contentParser).optional

  final override def all: Parser[Seq[A]] =
    Element(elementName, contentType, contentParser).all

  final def parse(from: From): Parser[A] =
    from.parse(contentType, Xml.withName(elementName, contentParser))

  // TODO move elsewhere
  final def descendants(xml: Node): Seq[A] =
    XmlUtil.descendants(xml, elementName).map(xml =>
      Parser.parseDo(parse(From.xml(xml)))
    )
}
