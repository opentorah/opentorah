package org.opentorah.tei

import org.opentorah.xml.{Element, ToXml}
import scala.xml.{Elem, Node}

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

  def apply(publisher: Seq[Node], availability: Availability): PublicationStmt = new PublicationStmt(
    publisher = Some(new Publisher.Value(publisher)),
    availability = Some(availability)
  )

  override def toXml(value: PublicationStmt): Elem =
    <publicationStmt>
      {Publisher.parsable.toXml(value.publisher)}
      {Availability.toXml(value.availability)}
    </publicationStmt>
}
