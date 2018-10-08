package org.podval.judaica.metadata.tanach

final case class ChumashSpan(
  override val book: Tanach.ChumashBook,
  override val span: Span
) extends BookSpan[Tanach.ChumashBook](book, span)
