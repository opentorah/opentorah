package org.opentorah.tei

import org.opentorah.xml.Descriptor
import scala.xml.{Elem, Node}

final case class PublicationStmt(
  publisher: Option[Publisher],
  availability: Option[Availability]
)

object PublicationStmt extends Descriptor[PublicationStmt](
  elementName = "publicationStmt",
  contentParser = for {
    publisher <- Publisher.optional
    availability <- Availability.optional
  } yield new PublicationStmt(
    publisher,
    availability
  )
) {

  def apply(publisher: Seq[Node], availability: Availability): PublicationStmt = new PublicationStmt(
    publisher = Some(Publisher(publisher)),
    availability = Some(availability)
  )

  override def toXml(value: PublicationStmt): Elem =
    <publicationStmt>
      {Publisher.toXml(value.publisher)}
      {Availability.toXml(value.availability)}
    </publicationStmt>
}
