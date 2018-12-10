package org.podval.judaica.metadata.tanach

final case class SpanSemiResolved(from: Verse, to: Option[Verse]) {
  def setTo(value: Verse): Span = {
    require(to.isEmpty || to.contains(value), "Wrong explicit 'to'")
    Span(from, value)
  }
}
