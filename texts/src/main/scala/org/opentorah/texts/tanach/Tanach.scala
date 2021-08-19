package org.opentorah.texts.tanach

import org.opentorah.metadata.{Metadata, Named, NamedCompanion, Names}
import org.opentorah.util.Collections
import org.opentorah.xml.{Element, From, Parsable, Parser, Unparser}

object Tanach extends NamedCompanion {

  trait Book extends Named {
    final def chapters: Chapters = {
      if (book2chapters.isEmpty) loadMetadata()
      book2chapters.get(this)
    }

    def parser(names: Names, chapters: Chapters): Parser[Parsed]

    def metadata: BookMetadata = forBook(this)
  }

  class BookMetadata(
    val book: Book
  )

  abstract class Parsed(
    val book: Book,
    val names: Names,
    val chapters: Chapters
  ) {
    def resolve: Parser[BookMetadata]
  }

  private object Parsed extends Element[Parsed]("book") {
    override def contentParsable: Parsable[Parsed] = new Parsable[Parsed] {
      override def parser: Parser[Parsed] = for {
        names <- Names.withDefaultNameParsable()
        chapters <- Chapters.parser
        book <- Metadata.find[Book](values, names)
        result <- book.parser(names, chapters)
      } yield result

      override def unparser: Unparser[Parsed] = ???
    }
  }

  override type Key = Book

  override lazy val toNames: Map[Book, Names] = {
    if (book2names.isEmpty) loadMetadata()
    book2names.get
  }

  override val values: Seq[Book] = Chumash.all ++ Nach.all

  private var book2names: Option[Map[Book, Names]] = None

  private var book2chapters: Option[Map[Book, Chapters]] = None

  private var book2metadata: Option[Map[Book, BookMetadata]] = None

  def forBook(book: Book): BookMetadata = {
    if (book2metadata.isEmpty) loadMetadata()
    book2metadata.get(book)
  }

  private def loadMetadata(): Unit = {
    val metadata: Map[Book, Parsed] = Parser.unsafeRun(
      Metadata.load(
        from = From.resource(Tanach),
        content = Parsed.followRedirects
      ).flatMap(Metadata.bind[Book, Parsed](
        keys = values,
        _,
        getKey = _.book
      ))
    )

    book2names = Some(Collections.mapValues(metadata)(_.names))
    book2chapters = Some(Collections.mapValues(metadata)(_.chapters))

    book2metadata = Some(Collections.mapValues(metadata)(metadata => Parser.unsafeRun(metadata.resolve)))
  }
}
