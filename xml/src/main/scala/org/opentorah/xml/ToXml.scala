package org.opentorah.xml

trait ToXml[A] {
  // TODO eliminate?
  private val toXml: Antiparser[A] = Antiparser(
    content = value => Seq(toXmlElement(value))
  )

  final def toXml[B](f: B => A): Antiparser[B] = toXml.compose(f)

  // TODO eliminate?
  private final val toXmlOption: Antiparser[Option[A]] = Antiparser(
    content = _.toSeq.map(toXmlElement)
  )

  // TODO eliminate?
  final def toXmlOption[B](f: B => Option[A]): Antiparser[B] = toXmlOption.compose(f)

  final val toXmlSeq: Antiparser[Seq[A]] = Antiparser(
    content = _.map(toXmlElement)
  )

  final def toXmlSeq[B](f: B => Seq[A]): Antiparser[B] = toXmlSeq.compose(f)

  def toXmlElement(value: A): Xml.Element
}
