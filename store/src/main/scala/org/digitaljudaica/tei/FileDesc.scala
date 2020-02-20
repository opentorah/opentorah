package org.digitaljudaica.tei

import cats.implicits._
import org.digitaljudaica.xml.Descriptor

final case class FileDesc(
  titleStmt: Option[TitleStmt],
  publicationStmt: PublicationStmt,
  sourceDesc: SourceDesc
)

object FileDesc extends Descriptor[FileDesc](
  elementName = "fileDesc",
  contentParser = for {
    titleStmt <- TitleStmt.optional
    publicationStmt <- PublicationStmt.required
    sourceDesc <- SourceDesc.required
  } yield new FileDesc(
    titleStmt,
    publicationStmt,
    sourceDesc
  )
)
