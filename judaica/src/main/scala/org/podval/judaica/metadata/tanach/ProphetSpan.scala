package org.podval.judaica.metadata.tanach

final case class ProphetSpan(
  override val book: Tanach.ProphetsBook,
  override val span: Span
) extends BookSpan[Tanach.ProphetsBook](book, span)

object ProphetSpan extends BookSpanCompanion {
  override type BookType = Tanach.ProphetsBook

  override type SpanType = ProphetSpan

  override protected def getBook(name: String): Tanach.ProphetsBook = Tanach.getProhetForName(name)

  override protected def create(book: Tanach.ProphetsBook, span: Span): ProphetSpan = ProphetSpan(book, span)
}
