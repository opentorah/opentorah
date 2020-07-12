package org.opentorah.xml

import scala.xml.{Elem, MetaData, Node, Null, UnprefixedAttribute}

trait ToXml[A] {
  final def toXml(value: Option[A]): Seq[Elem] = toXml(value.toSeq)

  final def toXml(values: Seq[A]): Seq[Elem] = values.map(toXml)

  // TODO create it without copying:
  final def toXml(value: A): Elem =
    <elem>{content(value)}</elem>.copy(
      label = elementName(value),
      attributes = attributes(value).foldRight[MetaData](Null){ case (current, result) => new UnprefixedAttribute(
        current.attribute.name,
        current.valueToString.orNull,
        result
      )}
    )

  protected def elementName(value: A): String

  protected def attributes(value: A): Seq[Attribute.Value[_]]

  protected def content(value: A): Seq[Node]
}
