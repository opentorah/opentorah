package org.podval.calendar.generate.tanach

import java.net.URL

import Tanach.{ChumashBook, ChumashBookStructure, NachBook, NachBookStructure}
import org.podval.calendar.metadata.MetadataParser.{MetadataPreparsed, bind}
import org.podval.calendar.metadata.{MetadataParser, Named, XML}

object TanachMetadataParser {
  def parse(obj: AnyRef): (
    Map[ChumashBook, ChumashBookStructure],
    Map[NachBook, NachBookStructure]
  ) = {
    val className: String = Named.className(obj)
    val url = MetadataParser.getUrl(obj, className)

    val books: Seq[MetadataPreparsed] = MetadataParser.loadMetadata(url, className, "book")

    (
      bind(Tanach.chumash, books.take(Tanach.chumash.length)).map { case (book, metadata) =>
        book -> parseChumashBook(url, Parsha.forBook(book), metadata)
      }.toMap,

      bind(Tanach.nach, books.takeRight(Tanach.nach.length)).map { case (book, metadata) =>
        book -> parseNachBook(metadata)
      }.toMap
    )
  }

  private def parseChumashBook(
    url: URL,
    parshiot: Seq[Parsha],
    metadata: MetadataPreparsed
  ): ChumashBookStructure = {
    metadata.attributes.close()
    // TODO handle names from metadata
    val (chapterElements, weekElements) = XML.span(metadata.elements, "chapter", "week")
    val chapters: Chapters = ChaptersParser.parse(chapterElements)

    val weeks: Seq[(Parsha, Parsha.Structure)] = ParshaMetadataParser.parse(
      metadata = MetadataParser.preparseMetadata(url, weekElements, "week"),
      parshiot = parshiot,
      chapters = chapters
    )

    new ChumashBookStructure(
      weeks.head._2.names,
      chapters,
      weeks.toMap
    )
  }

  private def parseNachBook(metadata: MetadataPreparsed): NachBookStructure = {
    metadata.attributes.close()
    val (chapterElements, tail) = XML.span(metadata.elements, "chapter")
    XML.checkNoMoreElements(tail)

    new NachBookStructure(
      metadata.names,
      chapters = ChaptersParser.parse(chapterElements)
    )
  }
}
