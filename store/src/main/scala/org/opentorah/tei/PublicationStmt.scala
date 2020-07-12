package org.opentorah.tei

import org.opentorah.xml.{Attribute, Element, Parser}
import scala.xml.Elem

final case class PublicationStmt(
  publisher: Option[Publisher.Value],
  availability: Option[Availability]
)

object PublicationStmt extends Element.WithToXml[PublicationStmt]("publicationStmt") {

  override protected def parser: Parser[PublicationStmt] = for {
    publisher <- Publisher.parsable.optional
    availability <- Availability.optional
  } yield new PublicationStmt(
    publisher,
    availability
  )

  override protected def attributes(value: PublicationStmt): Seq[Attribute.Value[_]] = Seq.empty

  override protected def content(value: PublicationStmt): Seq[Elem] =
    Publisher.parsable.toXml(value.publisher) ++
    Availability.toXml(value.availability)

  def apply(): PublicationStmt = new PublicationStmt(
    publisher = None,
    availability = None
  )
}
