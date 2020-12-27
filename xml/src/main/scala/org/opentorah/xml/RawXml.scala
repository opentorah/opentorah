package org.opentorah.xml

class RawXml(elementName: String, namespace: Option[Namespace] = None) {

  final class Value(val xml: Seq[Xml.Node]) {
    def xml2string: String = Xml.toString(xml)
  }

  object parsable extends Element[Value](elementName) {

    override def toString: String = s"raw element ${RawXml.this.elementName}"

    override def contentType: ContentType = ContentType.Mixed

    override def parser: Parser[Value] = Element.allNodes.map(new Value(_))

    override def antiparser: Antiparser[Value] = Antiparser(
      content = _.xml,
      namespace = namespace
    )
  }
}

object RawXml {

  def getXml(value: Option[RawXml#Value]): Seq[Xml.Node] = value.map(_.xml).getOrElse(Seq.empty)
}
