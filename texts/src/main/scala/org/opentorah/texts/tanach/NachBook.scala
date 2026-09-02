package org.opentorah.texts.tanach

import org.opentorah.metadata.Names
import org.opentorah.util.Effects
import org.podval.xml.XmlAst
import zio.ZIO

trait NachBook extends TanachBook:
  final override def names: Names = TanachBook.names(this)

  override def parse[E: XmlAst](names: Names, chapters: Chapters, element: E): NachBook.Parsed =
    XmlDecode.requireNoOther(element, Set("name", "chapter"))
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
