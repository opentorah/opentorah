package org.opentorah.xml

import scala.xml.{Elem, Node}

trait ToXml[A] {

  protected def elementName(value: A): String

  protected def antiparser: Antiparser[A]

  final def toXmlElement(value: A): Elem = ToXml.mkElement(
    elementName(value),
    antiparser.attributes(value),
    antiparser.content(value)
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

object ToXml {

  private def mkElement(
    name: String,
    attributes: Seq[Attribute.Value[_]],
    content: Seq[Node]
  ): Xml.Element = <elem/>.copy(
    label = name,
    attributes = attributes.foldRight[scala.xml.MetaData](scala.xml.Null){
      case (current, result) => new scala.xml.UnprefixedAttribute(
        current.attribute.name,
        current.valueToString.orNull,
        result
      )
    },
    child = content
  )
}
