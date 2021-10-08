package org.opentorah.texts.tanach

import org.opentorah.metadata.{HasName, HasValues, Names}
import org.opentorah.store.{By, Selector, Store, Stores}
import org.opentorah.util.{Collections, Platform}
import org.opentorah.xml.{Element, From, Parsable, Parser, ScalaXml, Unparser}

abstract class TanachBook(nameOverride: Option[String])
  extends Store.NonTerminal[Store], Stores.Pure[Store], HasName(nameOverride) derives CanEqual: // all deriveds are objects; using eq

  final def chapters: Chapters = TanachBook.book2chapters(this)

  override def storesPure: Seq[Store.NonTerminal[Store]] = Seq(chapters)

  def parser(names: Names, chapters: Chapters): Parser[TanachBook.Parsed]

object TanachBook extends Names.Loader[TanachBook], HasValues.FindByName[TanachBook], By[TanachBook], Stores.Pure[TanachBook]:
  private type Book = TanachBook

  override val valuesSeq: Seq[Book] = Chumash.all ++ Nach.all

  override def selector: Selector = Selector.byName("book")
  override def storesPure: Seq[Book] = valuesSeq

  private val book2parsed: Map[Book, Parsed] = Parser.unsafeRun(
    HasName.load(
      from = From.resource(Tanach),
      content = Parsed.followRedirects
    ).flatMap(HasName.bind[Book, Parsed](
      keys = valuesSeq,
      _,
      getKey = _.book
    ))
  )

  override lazy val toNames: Map[Book, Names] = Collections.mapValues(book2parsed)(_.names)

  private val book2chapters: Map[Book, Chapters] = Collections.mapValues(book2parsed)(_.chapters)

  private val book2metadata: Map[Book, Metadata] = Collections.mapValues(book2parsed)(metadata => Parser.unsafeRun(metadata.resolve))

  open class Metadata(
    val book: Book
  )

  def metadata(book: Book): Metadata = book2metadata(book)

  abstract class Parsed(
    val book: Book,
    val names: Names,
    val chapters: Chapters
  ):
    def resolve: Parser[Metadata]
  
  private object Parsed extends Element[Parsed]("book"):
    override def contentParsable: Parsable[Parsed] = new Parsable[Parsed]:
      override def parser: Parser[Parsed] = for
        names: Names <- Names.withDefaultNameParsable()
        chapters: Chapters <- Chapters.parser
        book: Book <- HasName.find[Book](valuesSeq, names)
        result: Parsed <- book.parser(names, chapters)
      yield result

      override def unparser: Unparser[Parsed] = ???
