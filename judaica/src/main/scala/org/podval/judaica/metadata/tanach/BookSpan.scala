package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, LanguageSpec, Util}

class BookSpan[B <: Tanach.TanachBook](val book: B, val span: Span) {
  override def toString: String = toString(LanguageSpec.empty)

  def toString(spec: LanguageSpec): String = book.toString(spec) + " " + span.toString(spec)
}

trait BookSpanCompanion {
  type BookType <: Tanach.TanachBook

  type SpanType <: BookSpan[BookType]

  final def toString(spans: Seq[SpanType], spec: LanguageSpec): String =
    Util.group(spans, (span: SpanType) => span.book)
      .map { bookSpans =>
        bookSpans.head.book.toString(spec) + " " + bookSpans.map(_.span.toString(spec)).mkString(", ")
      }.mkString("; ")

  final class Parsed(
    val book: Option[String],
    val fromChapter: Option[Int],
    val fromVerse: Option[Int],
    val toChapter: Option[Int],
    val toVerse: Option[Int]
  ) {
    def inheritFrom(span: Parsed): Parsed = {
      require(this.book.isEmpty || span.book.isEmpty)
      require(this.fromChapter.isEmpty || span.fromChapter.isEmpty)
      require(this.fromVerse.isEmpty || span.fromVerse.isEmpty)
      require(this.toChapter.isEmpty || span.toChapter.isEmpty)
      require(this.toVerse.isEmpty || span.toVerse.isEmpty)

      new Parsed(
        book = this.book.orElse(span.book),
        fromChapter = this.fromChapter.orElse(span.fromChapter),
        fromVerse = this.fromVerse.orElse(span.fromVerse),
        toChapter = this.toChapter.orElse(span.toChapter),
        toVerse = this.toVerse.orElse(span.toVerse)
      )
    }

    def resolve: SpanType = {
      require(book.isDefined)
      require(fromChapter.isDefined)
      require(fromVerse.isDefined)

      val result = create(
        book = getBook(book.get),
        span = Span(
          from = Verse(
            chapter = fromChapter.get,
            verse = fromVerse.get
          ),
          to = Verse(
            chapter = toChapter.getOrElse(fromChapter.get),
            verse = toVerse.getOrElse(fromVerse.get)
          )
        )
      )

      verify(result)

      result
    }

    private def verify(result: SpanType): Unit =
      require(result.book.chapters.contains(result.span), s"Book ${result.book} doesn't contain span ${result.span}")
  }

  final def parse(attributes: Attributes): Parsed = new Parsed(
    book = attributes.get("book"),
    fromChapter = attributes.getInt("fromChapter"),
    fromVerse = attributes.getInt("fromVerse"),
    toChapter = attributes.getInt("toChapter"),
    toVerse = attributes.getInt("toVerse")
  )

  protected def getBook(name: String): BookType

  protected def create(book: BookType, span: Span): SpanType

  final class Numbered(
    override val n: Int,
    val span: SpanType
  ) extends WithNumber

  final def parseNumbered(attributes: Attributes, contextSpan: Parsed): Numbered = {
    val n: Int = attributes.doGetInt("n")
    val span: Parsed = parse(attributes)
    attributes.close()
    new Numbered(
      n = n,
      span = span.inheritFrom(contextSpan).resolve
    )
  }
}
