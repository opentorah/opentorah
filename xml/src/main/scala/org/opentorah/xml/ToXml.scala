package org.opentorah.xml

import scala.xml.Elem

trait ToXml[A] {

  protected def elementName(value: A): String

  protected def antiparser: Antiparser[A]

  final def toXmlElement(value: A): Elem = Xml.construct(
    name = elementName(value),
    namespace = antiparser.namespace,
    attributes = antiparser.attributes(value),
    children = antiparser.content(value)
  )

  final val toXml: Antiparser[A] = Antiparser(
    content = value => Seq(toXmlElement(value))
  )

  final val toXmlOption: Antiparser[Option[A]] = Antiparser(
    content = _.toSeq.map(toXmlElement)
  )

  final val toXmlSeq: Antiparser[Seq[A]] = Antiparser(
    content = _.map(toXmlElement)
  )
}
