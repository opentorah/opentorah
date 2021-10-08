package org.opentorah.texts.tanach

import org.opentorah.metadata.Names
import org.opentorah.xml.Parser
import zio.ZIO

abstract class Nach(nameOverride: Option[String]) extends TanachBook(nameOverride):
  final override def names: Names = TanachBook.toNames(this)

  override def parser(names: Names, chapters: Chapters): Parser[Nach.Parsed] = ZIO.succeed(Nach.Parsed(
    this,
    names,
    chapters
  ))

object Nach:

  open class Metadata(
    book: Nach
  ) extends TanachBook.Metadata(book)

  open class Parsed(
    book: Nach,
    names: Names,
    chapters: Chapters
  ) extends TanachBook.Parsed(book, names, chapters):

    override def resolve: Parser[Metadata] = ZIO.succeed(Metadata(
      book
    ))

  val all: Seq[TanachBook] = Prophets.values.toIndexedSeq ++ Writings.all
