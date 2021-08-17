package org.opentorah.xml

import zio.ZIO

class RawXml(
  elementName: String,
  namespace: Option[Namespace] = None,
  attributesAllowed: Boolean = false
) {

  // TODO rename 'xml' to 'content'.
  final class Value(val xml: Xml.Nodes, val attributes: Attribute.Values)

  def apply(xml: Xml.Nodes, attributes: Attribute.Values = Seq.empty): Value = new Value(xml, attributes)

  object element extends Element[Value](elementName) {

    override def toString: String = s"raw element ${RawXml.this.elementName}"

    override def contentType: ContentType = ContentType.Mixed

    override def contentParsable: Parsable[Value] = new Parsable[Value] {
      override def parser: Parser[Value] = for {
        attributes <- if (attributesAllowed) Element.allAttributes else ZIO.succeed(Seq.empty)
        xml <- Element.nodes()
      } yield new Value(
        xml,
        attributes
      )

      override def unparser: Unparser[Value] = Unparser(
        attributes = _.attributes,
        content = _.xml,
        namespace = namespace
      )
    }
  }
}
