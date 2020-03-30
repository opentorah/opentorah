package org.opentorah.tei

import org.opentorah.xml.{Element, ToXml}
import scala.xml.Elem

final case class PublicationStmt(
  publisher: Option[Publisher.Value],
  availability: Option[Availability]
)

object PublicationStmt extends Element[PublicationStmt](
  elementName = "publicationStmt",
  parser = for {
    publisher <- Publisher.parsable.optional
    availability <- Availability.optional
  } yield new PublicationStmt(
    publisher,
    availability
  )
) with ToXml[PublicationStmt] {

  override def toXml(value: PublicationStmt): Elem =
    <publicationStmt>
      {Publisher.parsable.toXml(value.publisher)}
      {Availability.toXml(value.availability)}
    </publicationStmt>

  def apply(): PublicationStmt = new PublicationStmt(
    publisher = None,
    availability = None
  )
}
