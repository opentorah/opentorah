package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.Attributes

final case class NumberedSpan(override val n: Int, span: Span.Parsed) extends WithNumber

object NumberedSpan {
  def parse(attributes: Attributes): NumberedSpan = new NumberedSpan(
    n = attributes.doGetInt("n"),
    span = Span.parse(attributes)
  )

  def addImplied1(
    spans: Seq[NumberedSpan],
    span: Span,
    chapters: Chapters
  ): Seq[NumberedSpan] = {
    val first = spans.head
    val implied: Seq[NumberedSpan] = if (first.n == 1) Seq.empty else Seq(new NumberedSpan(1, new Span.Parsed(
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
  ): Seq[Span] = Span.setImpliedTo(dropNumbers(WithNumber.checkNumber(spans, number, "span")), span, chapters)

  private def dropNumbers(spans: Seq[NumberedSpan]): Seq[Span.Parsed] = spans.map(_.span)
}
