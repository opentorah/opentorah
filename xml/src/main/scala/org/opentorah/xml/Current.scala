package org.opentorah.xml

import scala.xml.Elem
import zio.IO

private[xml] final case class Current(
  from: Option[From],
  name: String,
  attributes: Map[String, String],
  content: Content
) {
  def takeAttribute(attribute: String): Current.Next[Option[String]] =
    IO.succeed((copy(attributes = attributes - attribute), attributes.get(attribute)))

  def takeAllAttributes: Current.Next[Map[String, String]] =
    IO.succeed((copy(attributes = Map.empty), attributes))

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
    attributes = element.attributes.map(metadata => metadata.prefixedKey -> metadata.value.toString).toMap,
    content
  )
}
