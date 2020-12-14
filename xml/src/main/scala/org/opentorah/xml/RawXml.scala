package org.opentorah.xml

import scala.xml.Node

class RawXml(elementName: String, namespace: Option[Namespace] = None) {

  final class Value(val xml: Seq[Node])

  object parsable extends Element.WithToXml[Value](elementName) {

    override def toString: String = s"raw element ${RawXml.this.elementName}"

    override def contentType: ContentType = ContentType.Mixed

    override def parser: Parser[Value] = Element.allNodes.map(new Value(_))

    override protected def antiparser: Antiparser[Value] = Antiparser(
      content = _.xml,
      namespace = namespace
    )
  }
}

object RawXml {

  def getXml(value: Option[RawXml#Value]): Seq[Node] = value.map(_.xml).getOrElse(Seq.empty)
}
