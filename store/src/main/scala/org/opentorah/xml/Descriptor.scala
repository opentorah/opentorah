package org.opentorah.xml

import scala.xml.Node

abstract class Descriptor[A](
  val elementName: String,
  val contentType: ContentType = ContentType.Elements,
  val contentParser: Parser[A] // TODO rename 'parser'?
) extends Element[A](
  elementName = Some(elementName),
  contentType,
  parser = contentParser
) with ToXml[A] {

  // TODO move elsewhere?
  final def parse(from: From): Parser[A] =
    from.parse(contentType, Xml.withName(elementName, contentParser))

  // TODO move elsewhere
  final def descendants(xml: Node): Seq[A] =
    XmlUtil.descendants(xml, elementName).map(xml =>
      Parser.parseDo(parse(From.xml(xml)))
    )
}
