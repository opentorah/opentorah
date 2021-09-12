package org.opentorah.xml

import zio.ZIO

open class RawXml(
  elementName: String,
  namespace: Option[Namespace] = None,
  attributesAllowed: Boolean = false
):

  final class Value(val content: ScalaXml.Nodes, val attributes: Attribute.Values)

  def apply(content: ScalaXml.Nodes, attributes: Attribute.Values = Seq.empty): Value = Value(content, attributes)

  object element extends Element[Value](elementName):

    override def toString: String = s"raw element ${RawXml.this.elementName}"

    override def contentType: ContentType = ContentType.Mixed

    override def contentParsable: Parsable[Value] = new Parsable[Value]:
      override def parser: Parser[Value] = for
        attributes: Attribute.Values <- if attributesAllowed then Attribute.allAttributes else ZIO.succeed(Seq.empty)
        xml: ScalaXml.Nodes <- Element.nodes()
      yield Value(
        xml,
        attributes
      )

      override def unparser: Unparser[Value] = Unparser(
        attributes = _.attributes,
        content = _.content,
        namespace = namespace
      )
