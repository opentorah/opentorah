package org.opentorah.xml

import scala.xml.Elem
import zio.IO

private[xml] final case class Current(
  from: Option[From],
  name: String,
  attributes: Seq[Attribute.Value[String]],
  content: Content
) {
  def takeAttribute(attribute: Attribute[_]): Current.Next[Option[String]] =
    IO.succeed((
      copy(attributes = attributes.filterNot(candidate => attribute == candidate.attribute)),
      // TODO handle repeated attributes?
      attributes.find(candidate => attribute == candidate.attribute).flatMap(_.value)
    ))

  def takeAllAttributes: Current.Next[Seq[Attribute.Value[String]]] =
    IO.succeed((
      copy(attributes = Seq.empty),
      attributes
    ))

  def checkNoLeftovers: Result = for {
    _ <- Parser.check(attributes.isEmpty, s"Unparsed attributes: $attributes")
    _ <- content.checkNoLeftovers
  } yield ()
}

private[xml] object Current {

  type Next[A] = IO[Error, (Current, A)]

  def open(from: Option[From], element: Elem, contentType: ContentType): IO[Error, Current] = for {
    content <- Content.open(element.child, contentType)
  } yield Current(
    from,
    name = element.label,
    attributes = Xml.getAttributes(element),
    content
  )
}
