package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{LanguageSpec, Util}

final case class ProphetSpan(book: Tanach.ProphetsBook, span: Span) {
  override def toString: String = toString(LanguageSpec.empty)

  def toString(spec: LanguageSpec): String = book.toString(spec) + " " + span.toString(spec)
}

object ProphetSpan {
  def toString(spans: Seq[ProphetSpan], spec: LanguageSpec): String =
    Util.group(spans, (span: ProphetSpan) => span.book)
      .map { bookSpans =>
        bookSpans.head.book.toString(spec) + " " + bookSpans.map(_.span.toString(spec)).mkString(", ")
      }.mkString("; ")
}
