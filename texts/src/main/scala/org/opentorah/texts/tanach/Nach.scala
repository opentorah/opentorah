package org.opentorah.texts.tanach

import org.opentorah.metadata.Names
import org.opentorah.xml.Parser
import zio.ZIO

trait Nach extends Tanach.Book {
  final override def names: Names = Tanach.toNames(this)

  override def parser(names: Names, chapters: Chapters): Parser[Nach.Parsed] = ZIO.succeed(new Nach.Parsed(
    this,
    names,
    chapters
  ))
}

object Nach {

  class BookMetadata(
    book: Nach
  ) extends Tanach.BookMetadata(book)

  class Parsed(
    book: Nach,
    names: Names,
    chapters: Chapters
  ) extends Tanach.Parsed(book, names, chapters) {

    override def resolve: Parser[BookMetadata] = ZIO.succeed(new BookMetadata(
      book
    ))
  }

  val all: Seq[Tanach.Book] = Prophets.all ++ Writings.all
}
