package org.opentorah.tei

import org.opentorah.xml.Descriptor
import scala.xml.{Elem, Node}

final case class FileDesc(
  titleStmt: TitleStmt,
  editionStmt: Option[EditionStmt],
  extent: Option[Extent],
  publicationStmt: PublicationStmt,
  seriesStmt: Option[SeriesStmt],
  notesStmt: Option[NotesStmt],
  sourceDesc: SourceDesc
)

object FileDesc extends Descriptor[FileDesc](
  elementName = "fileDesc",
  contentParser = for {
    titleStmt <- TitleStmt.required
    editionStmt <- EditionStmt.optional
    extent <- Extent.optional
    publicationStmt <- PublicationStmt.required
    seriesStmt <- SeriesStmt.optional
    notesStmt <- NotesStmt.optional
    sourceDesc <- SourceDesc.required
  } yield new FileDesc(
    titleStmt,
    editionStmt,
    extent,
    publicationStmt,
    seriesStmt,
    notesStmt,
    sourceDesc
  )
) {

  def apply(publicationStmt: PublicationStmt, sourceDesc: Seq[Node]): FileDesc = new FileDesc(
    titleStmt = TitleStmt(),
    editionStmt =  None,
    extent = None,
    publicationStmt,
    seriesStmt = None,
    notesStmt = None,
    sourceDesc = SourceDesc(sourceDesc)
  )

  override def toXml(value: FileDesc): Elem =
    <fileDesc>
      {TitleStmt.toXml(value.titleStmt)}
      {EditionStmt.toXml(value.editionStmt)}
      {Extent.toXml(value.extent)}
      {PublicationStmt.toXml(value.publicationStmt)}
      {SeriesStmt.toXml(value.seriesStmt)}
      {NotesStmt.toXml(value.notesStmt)}
      {SourceDesc.toXml(value.sourceDesc)}
    </fileDesc>
}
