package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, LanguageSpec, Util}

trait BookSpan[Book <: Tanach.TanachBook] {
  final case class BookSpan(book: Book, span: Span) {
    require(book.chapters.contains(span), s"Book $book doesn't contain span $span")

    override def toString: String = toString(LanguageSpec.empty)

    def toString(spec: LanguageSpec): String = book.toString(spec) + " " + span.toString(spec)
  }

  final def toString(spans: Seq[BookSpan], spec: LanguageSpec): String = {
    Util.group(spans, (span: BookSpan) => span.book)
      .map { bookSpans =>
        bookSpans.head.book.toString(spec) + " " + bookSpans.map(_.span.toString(spec)).mkString(", ")
      }.mkString("; ")
  }

  final case class Parsed(book: Option[String], span: Span.Parsed) {
    def inheritFrom(ancestor: Parsed): Parsed = {
      require(this.book.isEmpty || ancestor.book.isEmpty)

      Parsed(
        book = this.book.orElse(ancestor.book),
        span = this.span.inheritFrom(ancestor.span)
      )
    }

    def resolve: BookSpan = {
      require(book.isDefined)
      BookSpan(getBook(book.get), span.resolve)
    }
  }

  final def parse(attributes: Attributes): Parsed = Parsed(
    book = attributes.get("book"),
    Span.parse(attributes)
  )

  protected def getBook(name: String): Book

  final class Numbered(
    override val n: Int,
    val span: BookSpan
  ) extends WithNumber

  final def parseNumbered(attributes: Attributes, ancestorSpan: Parsed): Numbered = {
    val n: Int = attributes.doGetInt("n")
    val span: Parsed = parse(attributes)
    attributes.close()
    new Numbered(n, span = span.inheritFrom(ancestorSpan).resolve)
  }

  def dropNumbers(spans: Seq[Numbered]): Seq[BookSpan] = spans.map(_.span)
}

object BookSpan {
  object ChumashSpan extends BookSpan[Tanach.ChumashBook] {
    override protected def getBook(name: String): Tanach.ChumashBook = Tanach.getChumashForName(name)
  }

  object ProphetSpan extends BookSpan[Tanach.ProphetsBook] {
    override protected def getBook(name: String): Tanach.ProphetsBook = Tanach.getProhetForName(name)
  }
}
