package org.podval.calendar.generate.tanach

import org.podval.calendar.metadata.Attributes

object SpanParser {
  final class NumberedSpan(val n: Int, val span: SpanParsed)

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

  def checkNumber(spans: Seq[NumberedSpan], number: Int): Seq[NumberedSpan] = {
    require(spans.map(_.n) == (1 to number), "Wrong number of spans.")
    spans
  }

  def dropNumbers(spans: Seq[NumberedSpan]): Seq[SpanParsed] = spans.map(_.span)

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
}
