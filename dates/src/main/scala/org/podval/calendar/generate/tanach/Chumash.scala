package org.podval.calendar.generate.tanach

import org.podval.calendar.metadata.MetadataParser.MetadataPreparsed
import org.podval.calendar.metadata.{MetadataParser, Named, Names, WithKeyedMetadata,  MetadataLoader, XML}
import Parsha.Parsha

object Chumash extends WithKeyedMetadata with MetadataLoader  {

  sealed trait Book extends KeyBase {
    final def parshiot: Seq[Parsha] = Parsha.values.filter(_.book == this)
  }

  override type Key = Book

  final class BookStructure(
    override val names: Names,
    val chapters: Chapters,
    val weeks: Map[Parsha, Parsha.Structure]
  ) extends Named.NamedBase

  override type Metadata = BookStructure

  case object Genesis extends Book
  case object Exodus extends Book
  case object Leviticus extends Book
  case object Numbers extends Book
  case object Deuteronomy extends Book

  override val values: Seq[Book] = Seq(Genesis, Exodus, Leviticus, Numbers, Deuteronomy)

  protected override def elementName: String = "book"

  protected override def parseMetadata(book: Book, metadata: MetadataPreparsed): BookStructure = {
    metadata.attributes.close()
    val (chapterElements, weekElements) = XML.span(metadata.elements, "chapter", "week")
    val chapters: Chapters = Chapters(chapterElements)

    val weeks: Seq[(Parsha, Parsha.Structure)] = {
      val weeksCombined: Seq[ParshaMetadataParser.Combined] = ParshaMetadataParser.parse(
        metadata = weekElements.map(element => MetadataParser.loadSubresource(getUrl, element, "week")),
        chapters = chapters
      )
      val weeksBound: Seq[(Parsha, ParshaMetadataParser.Combined)] = Parsha.bind(book.parshiot, weeksCombined)
      weeksBound.map { case (parsha: Parsha, week: ParshaMetadataParser.Combined) => parsha -> week.squash(parsha, chapters) }
    }

    new BookStructure(
      weeks.head._2.names, // TODO Names.merge(weeks.head._2.names, metadata.names)
      chapters,
      weeks.toMap
    )
  }
}
