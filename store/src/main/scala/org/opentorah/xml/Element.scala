package org.opentorah.xml

import org.opentorah.xml
import scala.xml.Node

class Element[A](
  val elementName: String,
  contentType: ContentType = ContentType.Elements,
  parser: Parser[A]
) extends Parsable[A] {

  override def toString: String = s"element '$elementName'"

  final override val name2parser: Map[String, Parsable.ContentTypeAndParser[A]] =
    Map(elementName -> new xml.Parsable.ContentTypeAndParser[A](contentType, parser))

  // TODO lift into Parsable?
  final def descendants(xml: Node): Seq[A] =
    for (xml <- XmlUtil.descendants(xml, elementName))
    yield Parser.parseDo(parse(From.xml("descendants", xml)))
}

object Element {

  val allNodes: Parser[Seq[Node]] =
    Context.allNodes
}
