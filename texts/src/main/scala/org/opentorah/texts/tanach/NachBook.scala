package org.opentorah.texts.tanach

import org.opentorah.metadata.Names
import org.opentorah.util.Effects
import zio.ZIO

trait NachBook extends TanachBook:
  final override def names: Names = TanachBook.names(this)

  override def parse(names: Names, chapters: Chapters, dto: BookDto): NachBook.Parsed =
    require(dto.weeks.isEmpty && dto.days.isEmpty && dto.weekDays.isEmpty && dto.books.isEmpty)
    NachBook.Parsed(this, names, chapters)

object NachBook:

  open class Metadata(
    book: NachBook
  ) extends TanachBook.Metadata(book)

  open class Parsed(
    book: NachBook,
    names: Names,
    chapters: Chapters
  ) extends TanachBook.Parsed(book, names, chapters):
    override def resolve: Effects.IO[Metadata] = ZIO.succeed(Metadata(
      book
    ))
