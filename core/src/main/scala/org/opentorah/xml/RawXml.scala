package org.opentorah.xml

import zio.ZIO

open class RawXml(
  elementName: String,
  namespace: Option[Namespace] = None,
  attributesAllowed: Boolean = false
):

  final class Value(val content: Nodes, val attributes: Attribute.Values = Seq.empty)

  object element extends ElementTo[Value](elementName):

    override def toString: String = s"raw element ${RawXml.this.elementName}"

    override def contentType: ContentType = ContentType.Mixed

    override def contentParsable: Parsable[Value] = new Parsable[Value]:
      override def parser: Parser[Value] = for
        attributes: Attribute.Values <-
          if attributesAllowed 
          then ParserState.access(_.attributes) 
          else ZIO.succeed(Seq.empty)
        content: Nodes <- Nodes.all()
      yield Value(
        content,
        attributes
      )

      override def unparser: Unparser[Value] = Unparser(
        attributes = _.attributes,
        content = _.content,
        namespace = namespace
      )
