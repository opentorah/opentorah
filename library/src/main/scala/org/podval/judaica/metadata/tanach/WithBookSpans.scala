package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, LanguageSpec, LanguageString}

trait WithBookSpans[Book <: Tanach.TanachBook] {

  class Spans(val spans: Seq[BookSpan]) {
    final def length: Int = spans.length

    final def ++(that: Many): Many = apply(spans ++ that.spans)

    final def :+(that: BookSpan): Many = apply(spans :+ that)
  }

  protected type Many <: Spans

  def apply(spans: Seq[BookSpan]): Many

  final type Customs = Custom.Of[Many]

  final case class BookSpan(book: Book, span: Span) extends LanguageString {
    require(book.chapters.contains(span), s"Book $book doesn't contain span $span")

    override def toLanguageString(implicit spec: LanguageSpec): String =
      book.toLanguageString + " " + span.toLanguageString

    def +(next: BookSpan): BookSpan = merge(Seq(this, next))
  }

  final case class BookSpanParsed(book: Option[String], span: SpanParsed) {
    def inheritFrom(ancestor: BookSpanParsed): BookSpanParsed = {
      require(this.book.isEmpty || ancestor.book.isEmpty)

      BookSpanParsed(
        book = this.book.orElse(ancestor.book),
        span = this.span.inheritFrom(ancestor.span)
      )
    }

    def resolve: BookSpan = BookSpan(getBook(book.get), span.resolve)
  }

  final def parseSpan(attributes: Attributes): BookSpanParsed = {
    val result = BookSpanParsed(
      book = attributes.get("book"),
      SpanParsed.parse(attributes)
    )
    attributes.close()
    result
  }

  protected def getBook(name: String): Book

  def merge(spans: Seq[BookSpan]): BookSpan = {
    val book = spans.head.book
    require(spans.forall(_.book == book))
    require(book.chapters.consecutive(spans.map(_.span)))
    BookSpan(book = book, Span(spans.head.span.from, spans.last.span.to))
  }
}
