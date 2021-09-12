package org.opentorah.texts.tanach

import org.opentorah.metadata.{Language, LanguageSpec, Named}
import org.opentorah.xml.{Attribute, Parser}

trait WithBookSpans[Book <: Tanach.Book]:

  open class Spans(val spans: Seq[BookSpan]):
    final def length: Int = spans.length

    final def ++(that: Many): Many = apply(spans ++ that.spans)

    final def :+(that: BookSpan): Many = apply(spans :+ that)

    def from(source: Named): Many = apply(spans.map(_.from(source)))

  protected type Many <: Spans

  def apply(spans: Seq[BookSpan]): Many

  final type Customs = Custom.Of[Many]

  final class BookSpan private(val book: Book, val span: Span, val source: Option[Named]) extends Language.ToString:
    require(book.chapters.contains(span), s"Book $book doesn't contain span $span")

    override def toLanguageString(using spec: LanguageSpec): String =
      book.toLanguageString + " " + span.toLanguageString

    def +(next: BookSpan): BookSpan = merge(Seq(this, next))

    def from(source: Named): BookSpan = BookSpan(book, span, Some(source))

    override def hashCode(): Int = book.hashCode()*37 + span.hashCode() + 73

    override def equals(obj: Any): Boolean = obj.isInstanceOf[BookSpan] && {
      val that: BookSpan = obj.asInstanceOf[BookSpan]
      (this.book == that.book) && (this.span == that.span)
    }

  object BookSpan:
    def apply(book: Book, span: Span, source: Option[Named] = None): BookSpan = new BookSpan(book, span, source)

  final class BookSpanParsed(val book: Option[String], val span: SpanParsed):
    def inheritFrom(ancestor: BookSpanParsed): BookSpanParsed =
      require(this.book.isEmpty || ancestor.book.isEmpty)

      BookSpanParsed(
        book = this.book.orElse(ancestor.book),
        span = this.span.inheritFrom(ancestor.span)
      )

    def resolve: BookSpan = BookSpan(getBook(book.get), span.resolve)

  val spanParser: Parser[BookSpanParsed] = for
    book: Option[String] <- Attribute("book").optional()
    spanParsed: SpanParsed <- SpanParsed.parser
  yield BookSpanParsed(book, spanParsed)

  protected def getBook(name: String): Book

  def merge(spans: Seq[BookSpan]): BookSpan =
    val book = spans.head.book
    require(spans.forall(_.book == book))
    require(book.chapters.consecutive(spans.map(_.span)))

    val source: Option[Named] = spans.map(_.source).reduce[Option[Named]]((source1, source2) =>
      if source1.isEmpty && source2.isEmpty then None else
        require(source1.isDefined)
        require(source2.isDefined)
        Some(source1.get.merge(source2.get))
    )

    BookSpan(book = book, Span(spans.head.span.from, spans.last.span.to), source = source)
