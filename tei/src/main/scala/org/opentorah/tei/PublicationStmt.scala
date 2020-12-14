package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser}

final case class PublicationStmt(
  publisher: Option[Publisher.Value],
  availability: Option[Availability]
)

object PublicationStmt extends Element.WithToXml[PublicationStmt]("publicationStmt") {

  override val parser: Parser[PublicationStmt] = for {
    publisher <- Publisher.parsable.optional
    availability <- Availability.optional
  } yield new PublicationStmt(
    publisher,
    availability
  )

  override protected val antiparser: Antiparser[PublicationStmt] = Tei.concat(
    // TODO why do I need [PublicationStmt] for the compose() calls here?
    Publisher.parsable.toXmlOption.compose[PublicationStmt](_.publisher),
    Availability.toXmlOption.compose[PublicationStmt](_.availability)
  )

  def apply(): PublicationStmt = new PublicationStmt(
    publisher = None,
    availability = None
  )
}
