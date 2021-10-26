package org.opentorah.tei

import org.opentorah.xml.{Element, Parsable, Parser, Unparser}

// PublicationStmt and SourceDesc are mandatory (TEI Guidelines),
// but I made them optional so that they can be removed from the editable pre-TEI files
// (and added programmatically to the version published on the site).
final class FileDesc(
  val titleStmt: TitleStmt,
  val editionStmt: Option[EditionStmt.Value],
  val extent: Option[Extent.Value],
  val publicationStmt: Option[PublicationStmt],
  val seriesStmt: Option[SeriesStmt.Value],
  val notesStmt: Option[NotesStmt.Value],
  val sourceDesc: Option[SourceDesc.Value]
):
  def copy(
    titleStmt: TitleStmt = titleStmt,
    editionStmt: Option[EditionStmt.Value] = editionStmt,
    extent: Option[Extent.Value] = extent,
    publicationStmt: Option[PublicationStmt] = publicationStmt,
    seriesStmt: Option[SeriesStmt.Value] = seriesStmt,
    notesStmt: Option[NotesStmt.Value] = notesStmt,
    sourceDesc: Option[SourceDesc.Value] = sourceDesc
  ): FileDesc = FileDesc(
    titleStmt,
    editionStmt,
    extent,
    publicationStmt,
    seriesStmt,
    notesStmt,
    sourceDesc
  )

object FileDesc extends Element[FileDesc]("fileDesc"):

  override def contentParsable: Parsable[FileDesc] = new Parsable[FileDesc]:
    override def parser: Parser[FileDesc] = for
      titleStmt: TitleStmt <- TitleStmt.required()
      editionStmt: Option[EditionStmt.Value] <- EditionStmt.element.optional()
      extent: Option[Extent.Value] <- Extent.element.optional()
      publicationStmt: Option[PublicationStmt] <- PublicationStmt.optional()
      seriesStmt: Option[SeriesStmt.Value] <- SeriesStmt.element.optional()
      notesStmt: Option[NotesStmt.Value] <- NotesStmt.element.optional()
      sourceDesc: Option[SourceDesc.Value] <- SourceDesc.element.optional()
    yield FileDesc(
      titleStmt,
      editionStmt,
      extent,
      publicationStmt,
      seriesStmt,
      notesStmt,
      sourceDesc
    )

    override val unparser: Unparser[FileDesc] = Tei.concat(
      TitleStmt.required(_.titleStmt),
      EditionStmt.element.optional(_.editionStmt),
      Extent.element.optional(_.extent),
      PublicationStmt.optional(_.publicationStmt),
      SeriesStmt.element.optional(_.seriesStmt),
      NotesStmt.element.optional(_.notesStmt),
      SourceDesc.element.optional(_.sourceDesc)
    )

  def empty: FileDesc = FileDesc(
    titleStmt = TitleStmt.empty,
    editionStmt =  None,
    extent = None,
    publicationStmt = None,
    seriesStmt = None,
    notesStmt = None,
    sourceDesc = None
  )
