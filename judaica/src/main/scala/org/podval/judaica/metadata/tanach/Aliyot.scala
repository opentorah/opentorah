package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.LanguageSpec
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan

final case class Aliyot(span: ChumashSpan.BookSpan, aliyot: Seq[Span]) {
  // TODO verify that spans are consecutive and cover the book span

  override def toString: String = toString(LanguageSpec.empty)

  def toString(spec: LanguageSpec): String = aliyot.zipWithIndex.map { case (s, index) =>
    s"${index+1}: $s"
  }.mkString("\n")

  def getAliyot: Seq[ChumashSpan.BookSpan] = aliyot.map(_.inBook(span.book))
}

object Aliyot {

  def apply(
    bookSpan: ChumashSpan.BookSpan,
    aliyot: Seq[Span.Numbered],
    number: Option[Int]
  ): Aliyot = {
    val span = bookSpan.span
    val chapters = bookSpan.book.chapters
    val with1: Seq[Span.Numbered] = Span.addImplied1(aliyot, span, chapters)
    val spans: Seq[Span.Numbered] = WithNumber.checkNumber(with1, number.getOrElse(with1.length), "span")

    Aliyot(
      span = bookSpan,
      aliyot = Span.setImpliedTo(spans.map(_.span), span, chapters)
    )
  }
}
