package org.opentorah.tei

import org.opentorah.xml.{Element, ToXml}
import scala.xml.Elem

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

object FileDesc extends Element[FileDesc](
  elementName = "fileDesc",
  parser = for {
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
) with ToXml[FileDesc] {

  override def toXml(value: FileDesc): Elem =
    <fileDesc>
      {TitleStmt.toXml(value.titleStmt)}
      {EditionStmt.parsable.toXml(value.editionStmt)}
      {Extent.parsable.toXml(value.extent)}
      {PublicationStmt.toXml(value.publicationStmt)}
      {SeriesStmt.parsable.toXml(value.seriesStmt)}
      {NotesStmt.parsable.toXml(value.notesStmt)}
      {SourceDesc.parsable.toXml(value.sourceDesc)}
    </fileDesc>

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
