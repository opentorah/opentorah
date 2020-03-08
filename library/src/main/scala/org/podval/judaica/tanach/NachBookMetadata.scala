package org.podval.judaica.tanach

import org.opentorah.metadata.Names

final class NachBookMetadata(
  book: Tanach.NachBook
) extends TanachBookMetadata(book)

object NachBookMetadata {

  final class Parsed(
     book: Tanach.NachBook,
     names: Names,
     chapters: Chapters
  ) extends TanachBookMetadata.Parsed(book, names, chapters) {

    override def resolve: NachBookMetadata = new NachBookMetadata(
      book
    )
  }
}
