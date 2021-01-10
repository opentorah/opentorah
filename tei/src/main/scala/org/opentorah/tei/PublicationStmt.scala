package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parsable, Parser}

final case class PublicationStmt(
  publisher: Option[Publisher.Value],
  availability: Option[Availability]
)

object PublicationStmt extends Element[PublicationStmt]("publicationStmt") {

  override def contentParsable: Parsable[PublicationStmt] = new Parsable[PublicationStmt] {
    override val parser: Parser[PublicationStmt] = for {
      publisher <- Publisher.element.optional()
      availability <- Availability.optional()
    } yield new PublicationStmt(
      publisher,
      availability
    )

    override val antiparser: Antiparser[PublicationStmt] = Tei.concat(
      Publisher.element.optional(_.publisher),
      Availability.optional(_.availability)
    )
  }

  def empty: PublicationStmt = new PublicationStmt(
    publisher = None,
    availability = None
  )
}
