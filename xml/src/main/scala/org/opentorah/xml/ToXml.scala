package org.opentorah.xml

import scala.xml.{Elem, Node}

// TODO fold into Antiparser; move mkElement() here...
trait ToXml[A] {

  // TODO remove
  final def toXml(value: A): Elem = antiparser.mkElement(elementName(value), value)

  protected def elementName(value: A): String

  protected def antiparser: Antiparser[A]

  final def elementAntiparser: Antiparser[A] = Antiparser(
    content = value => Seq(toXml(value))
  )

  final def elementAntiparserOption: Antiparser[Option[A]] = Antiparser(
    content = _.toSeq.map(toXml)
  )

  final def elementAntiparserSeq: Antiparser[Seq[A]] = Antiparser(
    content = _.map(toXml)
  )
}
