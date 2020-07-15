package org.opentorah.texts.tanach

import org.opentorah.metadata.Names
import org.opentorah.xml.Parser
import zio.ZIO

final class NachBookMetadata(
  book: Tanach.NachBook
) extends TanachBookMetadata(book)

object NachBookMetadata {

  final class Parsed(
     book: Tanach.NachBook,
     names: Names,
     chapters: Chapters
  ) extends TanachBookMetadata.Parsed(book, names, chapters) {

    override def resolve: Parser[NachBookMetadata] = ZIO.succeed(new NachBookMetadata(
      book
    ))
  }
}
