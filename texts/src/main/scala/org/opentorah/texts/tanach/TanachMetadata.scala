package org.opentorah.texts.tanach

import Tanach.TanachBook
import org.opentorah.metadata.{Metadata, Names}
import org.opentorah.util.Collections
import org.opentorah.xml.{Unparser, Element, From, Parsable, Parser}
import zio.ZIO

object TanachMetadata {

  sealed trait State
  final case object Empty extends State
  final case class Parsed(
    names: Map[TanachBook, Names],
    chapters: Map[TanachBook, Chapters],
    metadata: Map[TanachBook, TanachBookMetadata.Parsed]
  ) extends State
  final case class Resolved(
    names: Map[TanachBook, Names],
    chapters: Map[TanachBook, Chapters],
    metadata: Map[TanachBook, TanachBookMetadata]
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

  def getChapters(book: TanachBook): Chapters = {
    ensureParsed()
    state match {
      case Parsed(_, chapters, _) => chapters(book)
      case Resolved(_, chapters, _) => chapters(book)
      case Empty => throw new IllegalStateException()
    }
  }

  private def ensureParsed(): Unit = {
    state match {
      case Parsed(_, _, _) =>
      case Resolved(_, _, _) =>
      case Empty => process {
        val metadata: Map[TanachBook, TanachBookMetadata.Parsed] = Parser.run(for {
          metadatas <- Metadata.load(
            from = From.resource(Tanach),
            content = Book.followRedirects
          )

          metadata <- Metadata.bind(
            keys = Tanach.values,
            metadatas,
            getKey = (metadata: TanachBookMetadata.Parsed) => metadata.book
          )
        } yield metadata)
        Parsed(
          Collections.mapValues(metadata)(_.names),
          Collections.mapValues(metadata)(_.chapters),
          metadata
        )
      }
    }
  }

  private object Book extends Element[TanachBookMetadata.Parsed]("book") {
    override def contentParsable: Parsable[TanachBookMetadata.Parsed] = new Parsable[TanachBookMetadata.Parsed] {
      override def parser: Parser[TanachBookMetadata.Parsed] = for {
        names <- Names.withDefaultNameParsable()
        chapters <- Chapters.parser
        book <- Metadata.find[TanachBook](Tanach.values, names)
        result <- book match {
          case book: Tanach.ChumashBook => ChumashBookMetadata.parser(book, names, chapters)
          case book: Tanach.Psalms.type => PsalmsMetadata.parser(book, names, chapters)
          case book: Tanach.NachBook    => ZIO.succeed(new NachBookMetadata.Parsed(book, names, chapters))
        }
      } yield result

      override def unparser: Unparser[TanachBookMetadata.Parsed] = ???
    }
  }

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
    metadata = Collections.mapValues(parsed.metadata)(metadata => Parser.run(metadata.resolve))
  )

  private var processing: Boolean = false

  // TODO I had to rework this for Scala 2.13 - it started breaking;
  // this probably means the usual: I am not good with concurrency;
  // it'd be best to redo this so that passes are cleanly delineated and concurrence is not used...
  private def process(f: => State): Unit = this.synchronized {
    if (processing) throw new IllegalStateException()
    processing = true
    state = f
    processing = false
  }
}
