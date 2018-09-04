package org.podval.calendar.generate.tanach

final case class Span(from: Verse, to: Verse) {
  require(from <= to)

  def contains(verse: Verse): Boolean = (from <= verse) && (verse <= to)

  // Assuming that Chapters.consecutive(this, that) returned 'true'.
  // def merge(that: Span): Span = Span(this.from, that.to)

  override def toString: String =
    if (from.chapter == to.chapter) s"${from.chapter}:${from.verse}-${to.verse}" else s"$from-$to"
}
