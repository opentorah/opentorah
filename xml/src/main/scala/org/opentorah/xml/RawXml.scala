package org.opentorah.xml

import scala.xml.Node

class RawXml(elementName: String) {

  final class Value(val xml: Seq[Node])

  object parsable extends Element.WithToXml[Value](elementName) {

    override def toString: String = s"raw element ${RawXml.this.elementName}"

    override protected def contentType: ContentType = ContentType.Mixed

    override protected def parser: Parser[Value] = Element.allNodes.map(new Value(_))

    override protected def attributes(value: Value): Seq[Attribute.Value[_]] = Seq.empty

    override protected def content(value: Value): Seq[Node] = value.xml
  }
}

object RawXml {

  def getXml(value: Option[RawXml#Value]): Seq[Node] = value.map(_.xml).getOrElse(Seq.empty)
}
