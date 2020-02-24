package org.digitaljudaica.tei

import org.digitaljudaica.xml.Descriptor

final case class FileDesc(
  titleStmt: Option[TitleStmt],
  publicationStmt: PublicationStmt,
  sourceDesc: Option[SourceDesc]
)

object FileDesc extends Descriptor[FileDesc](
  elementName = "fileDesc",
  contentParser = for {
    titleStmt <- TitleStmt.optional
    publicationStmt <- PublicationStmt.required
    sourceDesc <- SourceDesc.optional
  } yield new FileDesc(
    titleStmt,
    publicationStmt,
    sourceDesc
  )
)
