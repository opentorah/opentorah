package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser}

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

object FileDesc extends Element.WithToXml[FileDesc]("fileDesc") {

  override protected def parser: Parser[FileDesc] = for {
    titleStmt <- TitleStmt.required
    editionStmt <- EditionStmt.parsable.optional
    extent <- Extent.parsable.optional
    publicationStmt <- PublicationStmt.optional
    seriesStmt <- SeriesStmt.parsable.optional
    notesStmt <- NotesStmt.parsable.optional
    sourceDesc <- SourceDesc.parsable.optional
  } yield new FileDesc(
    titleStmt,
    editionStmt,
    extent,
    publicationStmt,
    seriesStmt,
    notesStmt,
    sourceDesc
  )

  override protected val antiparser: Antiparser[FileDesc] = Tei.concat(
    TitleStmt.toXml.compose(_.titleStmt),
    EditionStmt.parsable.toXmlOption.compose(_.editionStmt),
    Extent.parsable.toXmlOption.compose(_.extent),
    PublicationStmt.toXmlOption.compose(_.publicationStmt),
    SeriesStmt.parsable.toXmlOption.compose(_.seriesStmt),
    NotesStmt.parsable.toXmlOption.compose(_.notesStmt),
    SourceDesc.parsable.toXmlOption.compose(_.sourceDesc)
  )

  def apply(): FileDesc = new FileDesc(
    titleStmt = TitleStmt(),
    editionStmt =  None,
    extent = None,
    publicationStmt = None,
    seriesStmt = None,
    notesStmt = None,
    sourceDesc = None
  )
}
