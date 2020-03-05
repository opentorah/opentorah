package org.digitaljudaica.xml

import scala.xml.{Elem, Node}

class DescriptorRawXml[A <: RawXml](
  val elementName: String,
  create: Seq[Node] => A
) extends Parsable[A] with ToXml[A] {
  final override val required: Parser[A] =
    Raw(elementName).required.map(fromXml)

  final override val optional: Parser[Option[A]] =
    Raw(elementName).optional.map(_.map[A](fromXml))

  final override def all: Parser[Seq[A]] =
    Raw(elementName).all.map[Seq[A]](_.map(xml => fromXml(xml)))

  private def fromXml(xml: Elem): A = create(xml.child)

  final override def toXml(value: A): Elem = <elem>{value.getXml}</elem>.copy(label = elementName)
}
