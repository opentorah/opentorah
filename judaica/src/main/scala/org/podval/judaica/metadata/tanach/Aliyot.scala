package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.LanguageSpec
import org.podval.judaica.metadata.tanach.BookSpan.ChumashSpan

final case class Aliyot(bookSpan: ChumashSpan.BookSpan, aliyot: Seq[Span])

object Aliyot {

  // TODO unify with Torah from other places
  type Torah = Seq[ChumashSpan.BookSpan]

  def toString(torah: Torah): String = toString(torah, LanguageSpec.empty)

  def toString(torah: Torah, spec: LanguageSpec): String = torah.zipWithIndex.map { case (s, index) =>
    s"${index+1}: $s"
  }.mkString("\n")

  def parse(
    bookSpan: ChumashSpan.BookSpan,
    aliyotRaw: Seq[Span.Numbered],
    number: Option[Int]
  ): Torah = {
    val span = bookSpan.span
    val chapters = bookSpan.book.chapters
    val with1: Seq[Span.Numbered] = Span.addImplied1(aliyotRaw, span, chapters)
    val spans: Seq[Span.Numbered] = WithNumber.checkNumber(with1, number.getOrElse(with1.length), "span")
    Aliyot(bookSpan, Span.setImpliedTo(spans.map(_.span), span, chapters))
  }

  // TODO verify that spans are consecutive and cover the book span
  def apply(bookSpan: ChumashSpan.BookSpan, aliyot: Seq[Span]): Torah = aliyot.map(_.inBook(bookSpan.book))
}
