package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser}

final case class PublicationStmt(
  publisher: Option[Publisher.Value],
  availability: Option[Availability]
)

object PublicationStmt extends Element.WithToXml[PublicationStmt]("publicationStmt") {

  override protected val parser: Parser[PublicationStmt] = for {
    publisher <- Publisher.parsable.optional
    availability <- Availability.optional
  } yield new PublicationStmt(
    publisher,
    availability
  )

  override protected val antiparser: Antiparser[PublicationStmt] = Antiparser(
    // TODO why do I need [T] for the premap() calls here?
    Publisher.parsable.elementAntiparserOption.premap[PublicationStmt](_.publisher),
    Availability.elementAntiparserOption.premap[PublicationStmt](_.availability)
  )

  def apply(): PublicationStmt = new PublicationStmt(
    publisher = None,
    availability = None
  )
}
