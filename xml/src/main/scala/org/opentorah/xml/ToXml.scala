package org.opentorah.xml

trait ToXml[A] {
  final val toXml: Antiparser[A] = Antiparser(
    content = value => Seq(toXmlElement(value))
  )

  final val toXmlOption: Antiparser[Option[A]] = Antiparser(
    content = _.toSeq.map(toXmlElement)
  )

  final val toXmlSeq: Antiparser[Seq[A]] = Antiparser(
    content = _.map(toXmlElement)
  )

  def toXmlElement(value: A): Xml.Element
}
