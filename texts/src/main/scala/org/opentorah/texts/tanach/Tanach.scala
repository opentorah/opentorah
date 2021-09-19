package org.opentorah.texts.tanach

import org.opentorah.metadata.{Named, NamedCompanion, Names}
import org.opentorah.store.{By, Selector, Store, Stores}
import org.opentorah.util.Collections
import org.opentorah.xml.{Element, From, Parsable, Parser, Parsing, Unparser}

// TODO make Tanach a Store in Texts!
object Tanach extends NamedCompanion, Stores.Pure:

  override val values: Seq[Book] = Chumash.all ++ Nach.all

  final class ByBook extends By, Stores.Pure:
    override def selector: Selector = Selector.byName("book")
    override def storesPure: Seq[Book] = values

  override def storesPure: Seq[ByBook] = Seq(new ByBook)

  private val metadata: Map[Book, Parsed] = Parsing.unsafeRun(
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

  private val book2metadata: Map[Book, BookMetadata] = Collections.mapValues(metadata)(metadata => Parsing.unsafeRun(metadata.resolve))

  trait Book extends Store.NonTerminal, Stores.Pure:
    final def chapters: Chapters = book2chapters(this)

    override def storesPure: Seq[Store.NonTerminal] = Seq(chapters)

    def parser(names: Names, chapters: Chapters): Parser[Parsed]

    def metadata: BookMetadata = forBook(this)

  override type Key = Book

  open class BookMetadata(
    val book: Book
  )

  def forBook(book: Book): BookMetadata = book2metadata(book)

  abstract class Parsed(
    val book: Book,
    val names: Names,
    val chapters: Chapters
  ):
    def resolve: Parser[BookMetadata]

  private object Parsed extends Element[Parsed]("book"):
    override def contentParsable: Parsable[Parsed] = new Parsable[Parsed]:
      override def parser: Parser[Parsed] = for
        names: Names <- Names.withDefaultNameParsable()
        chapters: Chapters <- Chapters.parser
        book: Book <- Named.find[Book](values.toIndexedSeq, names)
        result: Parsed <- book.parser(names, chapters)
      yield result

      override def unparser: Unparser[Parsed] = ???
