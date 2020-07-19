package org.opentorah.xml

import scala.xml.{Elem, Node}

trait ToXml[A] {
  final def toXml(value: Option[A]): Seq[Elem] = toXml(value.toSeq)

  final def toXml(values: Seq[A]): Seq[Elem] = values.map(toXml)

  final def toXml(value: A): Elem = Xml.element(
    name = elementName(value),
    attributes = attributes(value),
    content = content(value)
  )

  protected def elementName(value: A): String

  protected def attributes(value: A): Seq[Attribute.Value[_]]

  protected def content(value: A): Seq[Node]
}
