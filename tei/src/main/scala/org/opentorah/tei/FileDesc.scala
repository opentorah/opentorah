package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parsable, Parser}

// PublicationStmt and SourceDesc are mandatory (TEI Guidelines),
// but I made them optional so that they can be removed from the editable pre-TEI files
// (and added programmatically to the version published on the site).
final case class FileDesc(
  titleStmt: TitleStmt,
  editionStmt: Option[EditionStmt.Value],
  extent: Option[Extent.Value],
  publicationStmt: Option[PublicationStmt],
  seriesStmt: Option[SeriesStmt.Value],
  notesStmt: Option[NotesStmt.Value],
  sourceDesc: Option[SourceDesc.Value]
)

object FileDesc extends Element[FileDesc]("fileDesc") {

  override def contentParsable: Parsable[FileDesc] = new Parsable[FileDesc] {
    override def parser: Parser[FileDesc] = for {
      titleStmt <- TitleStmt.required()
      editionStmt <- EditionStmt.element.optional()
      extent <- Extent.element.optional()
      publicationStmt <- PublicationStmt.optional()
      seriesStmt <- SeriesStmt.element.optional()
      notesStmt <- NotesStmt.element.optional()
      sourceDesc <- SourceDesc.element.optional()
    } yield new FileDesc(
      titleStmt,
      editionStmt,
      extent,
      publicationStmt,
      seriesStmt,
      notesStmt,
      sourceDesc
    )

    override val antiparser: Antiparser[FileDesc] = Tei.concat(
      TitleStmt.required(_.titleStmt),
      EditionStmt.element.optional(_.editionStmt),
      Extent.element.optional(_.extent),
      PublicationStmt.optional(_.publicationStmt),
      SeriesStmt.element.optional(_.seriesStmt),
      NotesStmt.element.optional(_.notesStmt),
      SourceDesc.element.optional(_.sourceDesc)
    )
  }

  def empty: FileDesc = new FileDesc(
    titleStmt = TitleStmt.empty,
    editionStmt =  None,
    extent = None,
    publicationStmt = None,
    seriesStmt = None,
    notesStmt = None,
    sourceDesc = None
  )
}
