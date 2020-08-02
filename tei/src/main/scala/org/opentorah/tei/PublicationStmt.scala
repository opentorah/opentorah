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
    content = value =>
      Publisher.parsable.toXml(value.publisher) ++
      Availability.toXml(value.availability)
  )

  def apply(): PublicationStmt = new PublicationStmt(
    publisher = None,
    availability = None
  )
}
