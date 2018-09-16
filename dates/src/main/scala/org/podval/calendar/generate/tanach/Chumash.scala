package org.podval.calendar.generate.tanach

import java.net.URL

import org.podval.calendar.metadata.MetadataParser.MetadataPreparsed
import org.podval.calendar.metadata.{MetadataParser, Names,WithKeyedMetadata,  WithMetadataLoading, XML}
import Parsha.Parsha

object Chumash extends WithKeyedMetadata with WithMetadataLoading  {

  sealed trait Book extends MetadataBase {
    final def parshiot: Seq[Parsha] = Parsha.values.filter(_.book == this)
  }

  override type Key = Book

  final class BookStructure(
    override val names: Names,
    val chapters: Chapters,
    val weeks: Map[Parsha, Parsha.Structure]
  ) extends Names.NamedBase

  override type Metadata = BookStructure

  case object Genesis extends Book
  case object Exodus extends Book
  case object Leviticus extends Book
  case object Numbers extends Book
  case object Deuteronomy extends Book

  override val values: Seq[Book] = Seq(Genesis, Exodus, Leviticus, Numbers, Deuteronomy)

  protected override def elementName: String = "book"

  protected override def parseMetadata(url: URL, book: Book, metadata: MetadataPreparsed): BookStructure = {
    metadata.attributes.close()
    val (chapterElements, weekElements) = XML.span(metadata.elements, "chapter", "week")
    val chapters: Chapters = Chapters(chapterElements)

    val weeks: Seq[(Parsha, Parsha.Structure)] = {
      val weeksCombined: Seq[ParshaMetadataParser.Combined] = ParshaMetadataParser.parse(
        metadata = MetadataParser.preparseMetadata(url, weekElements, "week"),
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
