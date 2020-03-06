package org.digitaljudaica.xml

import zio.ZIO

class Element[A](
  elementName: Option[String],
  contentType: ContentType,
  parser: Parser[A]
) extends Parsable[A] {

  override def toString: String = s"element $elementName"

  override def optional: Parser[Option[A]] = for {
    nextElementName <- Xml.nextName
    hasNext = elementName.fold(nextElementName.isDefined)(nextElementName.contains)
    result <- if (!hasNext) ZIO.none else for {
      next <- Parser.required(this.toString, Xml.nextElement)
      result <- Context.nested(None, next, contentType, parser)
    } yield Some(result)
  } yield result
}

object Element {
  def apply[A](name: String, contentType: ContentType, parser: Parser[A]): Parsable[A] =
    new Element(Some(name), contentType, parser)

  def apply[A](name: String, parser: Parser[A]): Parsable[A] =
    apply(name, ContentType.Elements, parser)

  // TODO eliminate uses without element name (NamesList); merge with Descriptor (and ElementRaw's optional()?).
//  def apply[A](contentType: ContentType, parser: Parser[A]): Parsable[A] =
//    new Element(None, contentType, parser)
  def apply[A](parser: Parser[A]): Parsable[A] =
//    apply(ContentType.Elements, parser)
    new Element(None, ContentType.Elements, parser)
}
