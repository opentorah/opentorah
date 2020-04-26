package org.opentorah.judaica.tanach

final case class SpanSemiResolved(from: Verse, to: Option[Verse]) {
  def setTo(value: Verse): Span = {
    require(to.isEmpty || to.contains(value), "Wrong explicit 'to'")
    Span(from, value)
  }
}

object SpanSemiResolved {
  def setImpliedTo(
    spans: Seq[SpanSemiResolved],
    span: Span,
    chapters: Chapters
  ): Seq[Span] = {
    val tos: Seq[Verse] = spans.tail.map(_.from).map(chapters.prev(_).get) :+ span.to
    val result = spans.zip(tos).map { case (s, to) => s.setTo(to) }
    require(chapters.cover(result, span))
    result
  }
}
