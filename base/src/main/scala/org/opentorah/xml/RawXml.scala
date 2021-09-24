package org.opentorah.xml

import zio.ZIO

open class RawXml(
  elementName: String,
  namespace: Option[Namespace] = None,
  attributesAllowed: Boolean = false
):

  final class Value(val content: Element.Nodes, val attributes: Attribute.Values = Seq.empty)

  object element extends Element[Value](elementName):

    override def toString: String = s"raw element ${RawXml.this.elementName}"

    override def contentType: Element.ContentType = Element.ContentType.Mixed

    override def contentParsable: Parsable[Value] = new Parsable[Value]:
      override def parser: Parser[Value] = for
        attributes: Attribute.Values <- if attributesAllowed then Attribute.allAttributes else ZIO.succeed(Seq.empty)
        content: Element.Nodes <- Element.nodes()
      yield Value(
        content,
        attributes
      )

      override def unparser: Unparser[Value] = Unparser(
        attributes = _.attributes,
        content = _.content.scalaXml,
        namespace = namespace
      )
