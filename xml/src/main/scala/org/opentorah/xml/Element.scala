package org.opentorah.xml

import scala.xml.Node

abstract class Element[A](
  val elementName: String
) extends Parsable[A] {

  override def toString: String = s"element '$elementName'"

  final override lazy val name2parser: Map[String, Parsable.ContentTypeAndParser[A]] =
    Map(elementName -> new Parsable.ContentTypeAndParser[A](contentType, parser))

  protected def contentType: ContentType = ContentType.Elements

  protected def parser: Parser[A]
}

object Element {

  abstract class WithToXml[A](elementName: String) extends Element[A](elementName) with ToXml[A] {
    final override protected def elementName(value: A): String = elementName
  }

  val allNodes: Parser[Seq[Node]] =
    Context.allNodes
}
