package org.opentorah.texts.tanach

final class SpanSemiResolved(val from: ChapterAndVerse, val to: Option[ChapterAndVerse]):
  def setTo(value: ChapterAndVerse): Span =
    require(to.isEmpty || to.contains(value), "Wrong explicit 'to'")
    Span(from, value)

object SpanSemiResolved:
  def setImpliedTo(
    spans: Seq[SpanSemiResolved],
    span: Span,
    chapters: Chapters
  ): Seq[Span] =
    val tos: Seq[ChapterAndVerse] = spans.tail.map(_.from).map(chapters.prev(_).get) :+ span.to
    val result = spans.zip(tos).map((s, to) => s.setTo(to))
    require(chapters.cover(result, span))
    result
