package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{MainMetadata, MetadataLoader, Named, Names, PreparsedMetadata, XML}
import org.podval.judaica.metadata.tanach.Parsha.Parsha

object Chumash extends MainMetadata with MetadataLoader  {

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

  protected override def parseMetadata(book: Book, metadata: PreparsedMetadata): BookStructure = {
    metadata.attributes.close()
    val (chapterElements, weekElements) = XML.span(metadata.elements, "chapter", "week")
    val chapters: Chapters = Chapters(chapterElements)

    val weeks: Map[Parsha, Parsha.Structure] = Parsha.parse(
      elements = weekElements,
      book = book,
      chapters = chapters
    )

    val names: Names = weeks(book.parshiot.head).names // TODO Names.merge(names, metadata.names)

    new BookStructure(
      names,
      chapters,
      weeks
    )
  }
}
