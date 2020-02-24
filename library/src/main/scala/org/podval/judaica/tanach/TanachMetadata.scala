package org.podval.judaica.tanach

import Tanach.TanachBook
import org.digitaljudaica.metadata.{Metadata, Names}
import org.digitaljudaica.util.Collections
import org.digitaljudaica.xml.{From, Parser, Xml}
import zio.IO

object TanachMetadata {

  sealed trait State
  final case object Empty extends State
  final case class Parsed(
    names: Map[Tanach.TanachBook, Names],
    chapters: Map[Tanach.TanachBook, Chapters],
    metadata: Map[Tanach.TanachBook, TanachBookMetadata.Parsed]
  ) extends State
  final case class Resolved(
    names: Map[Tanach.TanachBook, Names],
    chapters: Map[Tanach.TanachBook, Chapters],
    metadata: Map[Tanach.TanachBook, TanachBookMetadata]
  ) extends State

  private var state: State = Empty

  def getBook2names: Map[TanachBook, Names] = {
    ensureParsed()
    state match {
      case Parsed(names, _, _) => names
      case Resolved(names, _, _) => names
      case Empty => throw new IllegalStateException()
    }
  }

  def getChapters(book: Tanach.TanachBook): Chapters = {
    ensureParsed()
    state match {
      case Parsed(_, chapters, _) => chapters(book)
      case Resolved(_, chapters, _) => chapters(book)
      case Empty => throw new IllegalStateException()
    }
  }

  private def ensureParsed(): Unit = {
    checkNotProcessing()
    state match {
      case Parsed(_, _, _) =>
      case Resolved(_, _, _) =>
      case Empty => process(parse)
    }
  }

  private def parse: Parsed = {
    val metadatas: Seq[TanachBookMetadata.Parsed] = Metadata.load(
      from = From.resource(Tanach),
      elementName = "book",
      parser = Xml.withInclude(bookParser)
    )

    // TODO check that there is only one name (default) for the Chumash book and that it is among the names of its first parsha

    val metadata: Map[Tanach.TanachBook, TanachBookMetadata.Parsed] =
      Metadata.toMap(Tanach.values, metadatas, (metadata: TanachBookMetadata.Parsed) =>  metadata.book)

    Parsed(
      Collections.mapValues(metadata)(_.names),
      Collections.mapValues(metadata)(_.chapters),
      metadata
    )
  }

  private def bookParser: Parser[TanachBookMetadata.Parsed] = for {
    names <- Names.withDefaultNameParser
    chapters <- Chapters.parser
    book = Metadata.find[Tanach.TanachBook, Names](Tanach.values, names)
    result <- book match {
      case book: Tanach.ChumashBook => ChumashBookMetadata.parser(book, names, chapters)
      case book: Tanach.Psalms.type => PsalmsMetadata.parser(book, names, chapters)
      case book: Tanach.NachBook => IO.succeed(new NachBookMetadata.Parsed(book, names, chapters))
    }
  } yield result

  def forChumash(book: Tanach.ChumashBook): ChumashBookMetadata = forBook(book).asInstanceOf[ChumashBookMetadata]

  def forPsalms: PsalmsMetadata = forBook(Tanach.Psalms).asInstanceOf[PsalmsMetadata]

  def forBook(book: TanachBook): TanachBookMetadata = {
    ensureResolved()
    state match {
      case Resolved(_, _, metadata) => metadata(book)
      case Empty => throw new IllegalStateException()
      case Parsed(_, _, _) => throw new IllegalStateException()
    }
  }

  private def ensureResolved(): Unit = {
    checkNotProcessing()
    ensureParsed()
    state match {
      case parsed@Parsed(_, _, _) => process(resolve(parsed))
      case Resolved(_, _, _) =>
      case Empty => throw new IllegalStateException()
    }
  }

  private def resolve(parsed: Parsed): Resolved = Resolved(
    names = parsed.names,
    chapters = parsed.chapters,
    metadata = Collections.mapValues(parsed.metadata)(_.resolve)
  )

  private var processing: Boolean = false

  private def checkNotProcessing(): Unit = this.synchronized {
    if (processing) throw new IllegalStateException()
  }

  private def process(f: => State): Unit = this.synchronized {
    processing = true
    state = f
    processing = false
  }
}
