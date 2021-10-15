package org.opentorah.texts.tanach

import org.opentorah.metadata.Names
import org.opentorah.xml.Parser
import zio.ZIO

trait NachBook extends TanachBook:
  final override def names: Names = TanachBook.names(this)

  override def parser(names: Names, chapters: Chapters): Parser[NachBook.Parsed] =
    NachBook.parser(this, names, chapters)

object NachBook:

  def parser(book: NachBook, names: Names, chapters: Chapters): Parser[Parsed] = ZIO.succeed(Parsed(
    book,
    names,
    chapters
  ))

  open class Metadata(
    book: NachBook
  ) extends TanachBook.Metadata(book)

  open class Parsed(
    book: NachBook,
    names: Names,
    chapters: Chapters
  ) extends TanachBook.Parsed(book, names, chapters):
    override def resolve: Parser[Metadata] = ZIO.succeed(Metadata(
      book
    ))
