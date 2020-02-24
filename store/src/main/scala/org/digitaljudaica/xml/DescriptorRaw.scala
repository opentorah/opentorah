package org.digitaljudaica.xml

import scala.xml.Elem

class DescriptorRaw[A](
  val elementName: String,
  create: Elem => A
) {
  final val required: Parser[A] =
    Xml.required(elementName).map(fromXml)

  final val optional: Parser[Option[A]] =
    Xml.optional(elementName).map(_.map[A](fromXml))

  final def all: Parser[Seq[A]] =
    Xml.all(elementName).map[Seq[A]](_.map(xml => fromXml(xml)))

  private def fromXml(xml: Elem): A = create(xml)
}
