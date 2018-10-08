package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, XML}

import scala.xml.Elem

final class NumberedSpan(override val n: Int, val span: Span.Parsed) extends WithNumber

object NumberedSpan {
  def parse(element: Elem, name: String): NumberedSpan = {
    val attributes = XML.openEmpty(element, name)
    val result = parse(attributes)
    attributes.close()
    result
  }

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
  ): Seq[Span] = Span.setImpliedTo(dropNumbers(checkNumber(spans, number)), span, chapters)

  private def checkNumber(spans: Seq[NumberedSpan], number: Int): Seq[NumberedSpan] = {
    WithNumber.checkConsecutive(spans, "span")
    require(spans.length == number, "Wrong number of spans.")
    spans
  }

  private def dropNumbers(spans: Seq[NumberedSpan]): Seq[Span.Parsed] = spans.map(_.span)

  def overlaySpans(base: Seq[NumberedSpan], differences: Seq[NumberedSpan]): Seq[NumberedSpan] = {
    val result: Array[NumberedSpan] = base.toArray
    differences.foreach(span => result(span.n-1) = span)
    result.toSeq
  }
}
