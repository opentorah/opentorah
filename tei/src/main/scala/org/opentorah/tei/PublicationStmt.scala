package org.opentorah.tei

import org.opentorah.xml.{Element, Parsable, Parser, Unparser}

final class PublicationStmt(
  val publisher: Option[Publisher.Value],
  val availability: Option[Availability]
)

object PublicationStmt extends Element[PublicationStmt]("publicationStmt"):

  override def contentParsable: Parsable[PublicationStmt] = new Parsable[PublicationStmt]:
    override val parser: Parser[PublicationStmt] = for
      publisher: Option[Publisher.Value] <- Publisher.element.optional()
      availability: Option[Availability] <- Availability.optional()
    yield PublicationStmt(
      publisher,
      availability
    )

    override val unparser: Unparser[PublicationStmt] = Tei.concat(
      Publisher.element.optional(_.publisher),
      Availability.optional(_.availability)
    )

  def empty: PublicationStmt = PublicationStmt(
    publisher = None,
    availability = None
  )
