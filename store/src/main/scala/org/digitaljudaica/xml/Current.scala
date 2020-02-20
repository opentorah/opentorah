package org.digitaljudaica.xml

import cats.implicits._
import scala.xml.Elem

private[xml] final case class Current(
  from: Option[From],
  name: String,
  attributes: Map[String, String],
  content: Content
)

private[xml] object Current {

  // TODO monadize :)
  type Modifier[A] = Current => ErrorOr[(Current, A)]

  def open(from: Option[From], element: Elem, contentType: ContentType): ErrorOr[Current] = for {
    content <- Content.open(element.child, contentType)
  } yield Current(
    from,
    name = element.label,
    attributes = element.attributes.map(metadata => metadata.prefixedKey -> metadata.value.toString).toMap,
    content
  )

  // TODO monadize :)
  def lift[A](f: Content.Modifier[A]): Modifier[A] = (current: Current) =>
    f(current.content).map { _.leftMap(content => current.copy(content = content)) }

  def takeAttribute(attribute: String): Modifier[Option[String]] = (current: Current) =>
    Right((current.copy(attributes = current.attributes - attribute), current.attributes.get(attribute)))

  val takeAllAttributes: Modifier[Map[String, String]] = (current: Current) =>
    Right((current.copy(attributes = Map.empty), current.attributes))

  val checkNoLeftovers: Current => ErrorOr[Unit] = (current: Current) => for {
    _ <- if (current.attributes.isEmpty) Right(()) else Left(s"Unparsed attributes: ${current.attributes}")
    _ <- Content.checkNoLeftovers(current.content)
  } yield ()
}
