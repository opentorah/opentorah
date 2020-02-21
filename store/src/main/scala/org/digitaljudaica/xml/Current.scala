package org.digitaljudaica.xml

import scala.xml.Elem
import zio.IO

private[xml] final case class Current(
  from: Option[From],
  name: String,
  attributes: Map[String, String],
  content: Content
)

private[xml] object Current {

  type Modifier[A] = Current => IO[Error, (Current, A)]

  def open(from: Option[From], element: Elem, contentType: ContentType): IO[Error, Current] = for {
    content <- Content.open(element.child, contentType)
  } yield Current(
    from,
    name = element.label,
    attributes = element.attributes.map(metadata => metadata.prefixedKey -> metadata.value.toString).toMap,
    content
  )

  def takeAttribute(attribute: String): Modifier[Option[String]] = (current: Current) =>
    IO.succeed((current.copy(attributes = current.attributes - attribute), current.attributes.get(attribute)))

  val takeAllAttributes: Modifier[Map[String, String]] = (current: Current) =>
    IO.succeed((current.copy(attributes = Map.empty), current.attributes))

  val checkNoLeftovers: Current => IO[Error, Unit] = (current: Current) => for {
    _ <- if (current.attributes.isEmpty) IO.succeed(()) else IO.fail(s"Unparsed attributes: ${current.attributes}")
    _ <- Content.checkNoLeftovers(current.content)
  } yield ()
}
