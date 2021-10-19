package org.opentorah.texts.tanach

import org.opentorah.metadata.{HasName, HasValues, Names}
import org.opentorah.store.{By, Pure, Store}
import org.opentorah.util.Collections
import org.opentorah.xml.{Element, From, Parsable, Parser, ScalaXml, Unparser}

trait TanachBook extends HasName, Pure[?] derives CanEqual: // all deriveds are objects; using eq

  private[tanach] final def chapters: Chapters = TanachBook.chapters(this)

  override def storesPure: Seq[By[?]] = Seq(chapters.byChapter)

  private[tanach] def parser(names: Names, chapters: Chapters): Parser[TanachBook.Parsed]

private[tanach] object TanachBook:
  def valuesSeq: Seq[TanachBook] = Tanach.Book.valuesSeq

  private object Parsed extends Element[Parsed]("book"):
    override def contentParsable: Parsable[Parsed] = new Parsable[Parsed]:
      override def parser: Parser[Parsed] = for
        names: Names <- Names.withDefaultNameParsable()
        chapters: Chapters <- Chapters.parser
        book: TanachBook <- HasName.find[TanachBook](valuesSeq, names)
        result: Parsed <- book.parser(names, chapters)
      yield result

      override def unparser: Unparser[Parsed] = ???

  private val book2parsed: Map[TanachBook, Parsed] = Parser.unsafeRun(
    HasName.load(
      from = From.resource(Tanach),
      content = Parsed.followRedirects
    ).flatMap(HasName.bind[TanachBook, Parsed](
      keys = valuesSeq,
      _,
      getKey = _.book
    ))
  )

  def names(book: TanachBook): Names = book2parsed(book).names

  private def chapters(book: TanachBook): Chapters = book2parsed(book).chapters

  private val book2metadata: Map[TanachBook, Metadata] = Collections.mapValues(book2parsed)(metadata => Parser.unsafeRun(metadata.resolve))

  def metadata(book: TanachBook): Metadata = book2metadata(book)

  abstract class Metadata(
    val book: TanachBook
  )

  abstract class Parsed(
    val book: TanachBook,
    val names: Names,
    val chapters: Chapters
  ):
    def resolve: Parser[Metadata]

