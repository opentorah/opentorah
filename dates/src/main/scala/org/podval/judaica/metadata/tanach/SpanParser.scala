package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, XML}

import scala.xml.Elem

object SpanParser {
  final class NumberedSpan(override val n: Int, val span: SpanParsed) extends WithNumber

  def overlaySpans(base: Seq[NumberedSpan], differences: Seq[NumberedSpan]): Seq[NumberedSpan] = {
    val result: Array[NumberedSpan] = base.toArray
    differences.foreach(span => result(span.n-1) = span)
    result.toSeq
  }

  def parseNumberedSpan(element: Elem, name: String): NumberedSpan = {
    val attributes = XML.openEmpty(element, name)
    val result = parseNumberedSpan(attributes)
    attributes.close()
    result
  }

  def parseNumberedSpan(attributes: Attributes): NumberedSpan = new NumberedSpan(
    n = attributes.doGetInt("n"),
    span = parseSpan(attributes)
  )

  final class SpanParsed(val from: Verse, val to: Option[Verse]) {
    def setTo(value: Verse): Span = {
      require(to.isEmpty || to.contains(value), "Wrong explicit 'to'")
      Span(from, value)
    }
  }

  def parseSpan(element: Elem, name: String): SpanParsed = {
    val attributes = XML.openEmpty(element, name)
    val result = parseSpan(attributes)
    attributes.close()
    result
  }

  def parseSpan(attributes: Attributes): SpanParsed = {
    val from = parseFrom(attributes)
    val toChapter = attributes.getInt("toChapter")
    val toVerse = attributes.getInt("toVerse")
    val to = if (toVerse.isEmpty) {
      require(toChapter.isEmpty)
      None
    } else {
      Some(Verse(toChapter.getOrElse(from.chapter), toVerse.get))
    }
    new SpanParsed(from, to)
  }

  def parseFrom(attributes: Attributes): Verse = Verse(
    attributes.doGetInt("fromChapter"),
    attributes.doGetInt("fromVerse")
  )

  def addImplied1(
    spans: Seq[NumberedSpan],
    span: Span,
    chapters: Chapters
  ): Seq[NumberedSpan] = {
    val first = spans.head
    val implied: Seq[NumberedSpan] = if (first.n == 1) Seq.empty else Seq(new NumberedSpan(1, new SpanParsed(
      span.from,
      Some(chapters.prev(first.span.from).get)
    )))

    implied ++ spans
  }

  def setImpliedToCheckAndDropNumbers(
    spans: Seq[NumberedSpan],
    number: Int,
    span: Span,
    chapters: Chapters
  ): Seq[Span] = setImpliedTo(dropNumbers(checkNumber(spans, number)), span, chapters)

  private def checkNumber(spans: Seq[NumberedSpan], number: Int): Seq[NumberedSpan] = {
    WithNumber.checkConsecutive(spans, "span")
    require(spans.length == number, "Wrong number of spans.")
    spans
  }

  private def dropNumbers(spans: Seq[NumberedSpan]): Seq[SpanParsed] = spans.map(_.span)

  def setImpliedTo(
    spans: Seq[SpanParsed],
    span: Span,
    chapters: Chapters
  ): Seq[Span] = {
    val tos: Seq[Verse] = spans.tail.map(_.from).map(chapters.prev(_).get) :+ span.to
    val result = spans.zip(tos).map { case (span, to) => span.setTo(to) }
    require(chapters.cover(result, span))

    result
  }

  final class NachSpanParsed(
    val book: Option[String],
    val fromChapter: Option[Int],
    val fromVerse: Option[Int],
    val toChapter: Option[Int],
    val toVerse: Option[Int]
  ) {
    def inheritFrom(span: NachSpanParsed): NachSpanParsed = {
      require(this.book.isEmpty || span.book.isEmpty)
      require(this.fromChapter.isEmpty || span.fromChapter.isEmpty)
      require(this.fromVerse.isEmpty || span.fromVerse.isEmpty)
      require(this.toChapter.isEmpty || span.toChapter.isEmpty)
      require(this.toVerse.isEmpty || span.toVerse.isEmpty)

      new NachSpanParsed(
        book = this.book.orElse(span.book),
        fromChapter = this.fromChapter.orElse(span.fromChapter),
        fromVerse = this.fromVerse.orElse(span.fromVerse),
        toChapter = this.toChapter.orElse(span.toChapter),
        toVerse = this.toVerse.orElse(span.toVerse)
      )
    }

    def resolve: ProphetSpan = {
      require(book.isDefined)
      require(fromChapter.isDefined)
      require(fromVerse.isDefined)

      val result = ProphetSpan(
        book = Nach.getProhetForName(book.get),
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

    private def verify(result: ProphetSpan): Unit = {
      require(result.book.metadata.chapters.contains(result.span), s"Book ${result.book} doesn't contain span ${result.span}")
    }
  }

  def parseNachSpan(attributes: Attributes): NachSpanParsed = new NachSpanParsed(
    book = attributes.get("book"),
    fromChapter = attributes.getInt("fromChapter"),
    fromVerse = attributes.getInt("fromVerse"),
    toChapter = attributes.getInt("toChapter"),
    toVerse = attributes.getInt("toVerse")
  )

  final class NumberedProphetSpan(
    override val n: Int,
    val span: ProphetSpan
  ) extends WithNumber

  def parseNumberedNachSpan(attributes: Attributes, contextSpan: NachSpanParsed): NumberedProphetSpan = {
    val n: Int = attributes.doGetInt("n")
    val partSpan = parseNachSpan(attributes)
    attributes.close()
    val span = partSpan.inheritFrom(contextSpan)
    new NumberedProphetSpan(
      n = n,
      span = span.resolve
    )
  }
}
