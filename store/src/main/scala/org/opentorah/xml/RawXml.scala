package org.opentorah.xml

import scala.xml.{Elem, Node}

abstract class RawXml(xml: Seq[Node]) {
  final def getXml: Seq[Node] = xml
}

object RawXml {
  class Descriptor[A <: RawXml](
    val elementName: String,
    create: Seq[Node] => A
  ) extends ElementRaw[A](
    elementName,
    fromXml = (xml: Elem) => create(xml.child)
  ) with ToXml[A] {

    final override def toXml(value: A): Elem =
      <elem>{value.getXml}</elem>
        .copy(label = elementName)
  }
}
