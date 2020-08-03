package org.opentorah.xml

import scala.xml.{Elem, Node}

// TODO fold into Antiparser; move mkElement() here...
trait ToXml[A] {
  final def toXml(value: Option[A]): Seq[Elem] = toXml(value.toSeq)

  final def toXml(values: Seq[A]): Seq[Elem] = values.map(toXml)

  final def toXml(value: A): Elem = antiparser.mkElement(elementName(value), value)

  protected def elementName(value: A): String

  protected def antiparser: Antiparser[A]

  final def elementAntiparser: Antiparser[A] = Antiparser(
    content = value => Seq(toXml(value))
  )
}
