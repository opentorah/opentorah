package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser}

final case class PublicationStmt(
  publisher: Option[Publisher.Value],
  availability: Option[Availability]
)

object PublicationStmt extends Element[PublicationStmt]("publicationStmt") {

  override val parser: Parser[PublicationStmt] = for {
    publisher <- Publisher.parsable.optional
    availability <- Availability.optional
  } yield new PublicationStmt(
    publisher,
    availability
  )

  override val antiparser: Antiparser[PublicationStmt] = Tei.concat(
    Publisher.parsable.toXmlOption(_.publisher),
    Availability.toXmlOption(_.availability)
  )

  def apply(): PublicationStmt = new PublicationStmt(
    publisher = None,
    availability = None
  )
}
