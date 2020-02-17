package org.digitaljudaica.xml

import cats.implicits._

import scala.xml.Elem

private[xml] final class Current private(
  val from: Option[From],
  val name: String,
  attributes: Map[String, String],
  val content: Content
) {
  override def toString: String =
    s"in $name; $content;" + from.fold("")(url => s"  $url")

  def takeAttribute(attribute: String): ErrorOr[(Current, Option[String])] =
    Right((new Current(from, name, attributes - attribute, content), attributes.get(attribute)))

  def replaceContent[A](f: Content => ErrorOr[(Content, A)]): ErrorOr[(Current, A)] =
    f(content).map { _.leftMap(new Current(from, name, attributes, _)) }

  def checkNoLeftovers: ErrorOr[Unit] = for {
    _ <- if (attributes.isEmpty) Right(()) else Left(s"Unparsed attributes: $attributes")
    _ <- Content.checkNoLeftovers(content)
  } yield ()
}

private[xml] object Current {

  def open(from: Option[From], element: Elem, contentType: Content.Type): ErrorOr[Current] =
    Content.open(element.child, contentType).map { content =>
      new Current(
        from,
        name = element.label,
        attributes = element.attributes.map(metadata => metadata.key -> metadata.value.toString).toMap,
        content
      )
    }
}
