package org.digitaljudaica.xml

import scala.xml.Elem

abstract class Descriptor[A](
  val elementName: String,
  val contentType: ContentType = ContentType.Elements,
  val contentParser: Parser[A]
) {
  final def required: Parser[A] =
    Xml.required(elementName, contentType, contentParser)

  final def optional: Parser[Option[A]] =
    Xml.optional(elementName, contentType, contentParser)

  final def all: Parser[Seq[A]] =
    Xml.all(elementName, contentType, contentParser)

  final def fromXml(xml: Elem): A =
    From.xml(xml).parseDo(contentType, Xml.withName(elementName, contentParser))

  final def descendants(xml: Elem): Seq[A] =
    org.digitaljudaica.xml.Ops.descendants(xml, elementName).map(fromXml)
}
