package org.digitaljudaica.xml

import scala.xml.{Elem, Node}

abstract class Descriptor[A](
  val elementName: String,
  val contentType: ContentType = ContentType.Elements,
  val contentParser: Parser[A],
  val toXml: A => Elem
) {
  final def required: Parser[A] =
    Xml.required(elementName, contentType, contentParser)

  final def optional: Parser[Option[A]] =
    Xml.optional(elementName, contentType, contentParser)

  final def all: Parser[Seq[A]] =
    Xml.all(elementName, contentType, contentParser)

  final def parse(from: From): Parser[A] =
    from.parse(contentType, Xml.withName(elementName, contentParser))

  // TODO move elsewhere
  final def descendants(xml: Node): Seq[A] =
    XmlUtil.descendants(xml, elementName).map(xml =>
      Parser.parseDo(parse(From.xml(xml)))
    )

  // TODO add attribute helpers for optional and integer attributes...
  final def toXml(values: Seq[A]): Seq[Elem] = values.map(toXml)

  final def toXml(value: Option[A]): Seq[Elem] = toXml(value.toSeq)
}
