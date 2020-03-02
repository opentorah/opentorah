package org.digitaljudaica.tei

import org.digitaljudaica.xml.Descriptor

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
  ),
  toXml = (value: PublicationStmt) =>
    <publicationStmt>
      {Publisher.toXml(value.publisher)}
      {Availability.toXml(value.availability)}
    </publicationStmt>
)
