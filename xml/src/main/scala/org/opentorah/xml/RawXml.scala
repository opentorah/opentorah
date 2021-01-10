package org.opentorah.xml

class RawXml(elementName: String, namespace: Option[Namespace] = None) {

  final class Value(val xml: Seq[Xml.Node]) {
    def xml2string: String = Xml.toString(xml)
  }

  object element extends Element[Value](elementName) {

    override def toString: String = s"raw element ${RawXml.this.elementName}"

    override def contentType: ContentType = ContentType.Mixed

    override def contentParsable: Parsable[Value] = new Parsable[Value] {
      override def parser: Parser[Value] = Element.nodes().map(new Value(_))

      override def antiparser: Antiparser[Value] = Antiparser(
        content = _.xml,
        namespace = namespace
      )
    }
  }
}
