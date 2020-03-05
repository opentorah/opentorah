package org.digitaljudaica.xml

import zio.ZIO

// TODO eliminate uses without element name; fold into Descriptor
final class Element[A](name: Option[String], contentType: ContentType, parser: Parser[A]) extends Parsable[A] {
  override def toString: String = s"element $name"

  override def optional: Parser[Option[A]] = for {
    nextElementName <- Xml.nextName
    hasNext = name.fold(nextElementName.isDefined)(nextElementName.contains)
    result <- if (!hasNext) ZIO.none else for {
      next <- Raw().required
      result <- Context.nested(None, next, contentType, parser)
    } yield Some(result)
  } yield result
}

object Element {
  def apply[A](name: String, contentType: ContentType, parser: Parser[A]): Parsable[A] =
    new Element(Some(name), contentType, parser)

  def apply[A](contentType: ContentType, parser: Parser[A]): Parsable[A] =
    new Element(None, contentType, parser)

  def apply[A](name: String, parser: Parser[A]): Parsable[A] =
    apply(name, ContentType.Elements, parser)

  def apply[A](parser: Parser[A]): Parsable[A] =
    apply(ContentType.Elements, parser)
}
