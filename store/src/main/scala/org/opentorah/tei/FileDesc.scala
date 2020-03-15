package org.opentorah.tei

import org.opentorah.xml.Element
import scala.xml.{Elem, Node}

final case class FileDesc(
  titleStmt: TitleStmt,
  editionStmt: Option[EditionStmt.Value],
  extent: Option[Extent.Value],
  publicationStmt: PublicationStmt,
  seriesStmt: Option[SeriesStmt.Value],
  notesStmt: Option[NotesStmt.Value],
  sourceDesc: SourceDesc.Value
)

object FileDesc extends Element[FileDesc](
  elementName = "fileDesc",
  parser = for {
    titleStmt <- TitleStmt.required
    editionStmt <- EditionStmt.parsable.optional
    extent <- Extent.parsable.optional
    publicationStmt <- PublicationStmt.required
    seriesStmt <- SeriesStmt.parsable.optional
    notesStmt <- NotesStmt.parsable.optional
    sourceDesc <- SourceDesc.parsable.required
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
    sourceDesc = new SourceDesc.Value(sourceDesc)
  )

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
}
