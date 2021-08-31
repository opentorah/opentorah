package org.opentorah.texts.tanach

import org.opentorah.metadata.{Named, NamedCompanion, Names}
import org.opentorah.util.Collections
import org.opentorah.xml.{Element, From, Parsable, Parser, Unparser}

// TODO make this a Store!
object Tanach extends NamedCompanion {

  override val values: Seq[Book] = Chumash.all ++ Nach.all

  private val metadata: Map[Book, Parsed] = Parser.unsafeRun(
    Named.load(
      from = From.resource(Tanach),
      content = Parsed.followRedirects
    ).flatMap(Named.bind[Book, Parsed](
      keys = values,
      _,
      getKey = _.book
    ))
  )

  override lazy val toNames: Map[Book, Names] = Collections.mapValues(metadata)(_.names)

  private val book2chapters: Map[Book, Chapters] = Collections.mapValues(metadata)(_.chapters)

  private val book2metadata: Map[Book, BookMetadata] = Collections.mapValues(metadata)(metadata => Parser.unsafeRun(metadata.resolve))

  // TODO make this a Store!
  trait Book extends Named {
    final def chapters: Chapters = book2chapters(this)

    def parser(names: Names, chapters: Chapters): Parser[Parsed]

    def metadata: BookMetadata = forBook(this)
  }

  override type Key = Book

  class BookMetadata(
    val book: Book
  )

  def forBook(book: Book): BookMetadata = book2metadata(book)

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
        book <- Named.find[Book](values, names)
        result <- book.parser(names, chapters)
      } yield result

      override def unparser: Unparser[Parsed] = ???
    }
  }
}
