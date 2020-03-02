package org.digitaljudaica.xml

import scala.xml.{Elem, Node}

class DescriptorRawXml[A <: RawXml](
  val elementName: String,
  create: Seq[Node] => A
) {
  final val required: Parser[A] =
    Xml.required(elementName).map(fromXml)

  final val optional: Parser[Option[A]] =
    Xml.optional(elementName).map(_.map[A](fromXml))

  final def all: Parser[Seq[A]] =
    Xml.all(elementName).map[Seq[A]](_.map(xml => fromXml(xml)))

  private def fromXml(xml: Elem): A = create(xml.child)

  final def toXml(value: A): Elem = <elem>{value.getXml}</elem>.copy(label = elementName)

  final def toXml(value: Option[A]): Elem = value.map(toXml).orNull

  final def toXml(values: Seq[A]): Seq[Elem] = values.map(toXml)
}
