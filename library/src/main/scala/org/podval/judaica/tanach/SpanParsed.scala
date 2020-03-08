package org.podval.judaica.tanach

import org.opentorah.xml.Parser

final case class SpanParsed(from: VerseParsed, to: VerseParsed) {
  def inheritFrom(ancestor: SpanParsed): SpanParsed = SpanParsed(
    from = this.from.inheritFrom(ancestor.from),
    to = this.to.inheritFrom(ancestor.to)
  )

  def defaultFromChapter(fromChapter: Int): SpanParsed =
    SpanParsed(from = from.defaultChapter(fromChapter), to = to)

  def semiResolve: SpanSemiResolved = {
    val fromResolved = from.resolve
    SpanSemiResolved(fromResolved, semiResolveTo(fromResolved))
  }

  private def semiResolveTo(fromResolved: Verse): Option[Verse] = {
    require(to.verse.nonEmpty || to.chapter.isEmpty)

    if (to.verse.isEmpty) None else Some(Verse(
      chapter = resolveToChapter(fromResolved),
      verse = to.verse.get
    ))
  }

  def resolve: Span = {
    val fromResolved = from.resolve
    Span(fromResolved, resolveTo(fromResolved))
  }

  private def resolveTo(fromResolved: Verse): Verse = Verse(
    chapter = resolveToChapter(fromResolved),
    verse = to.verse.getOrElse(fromResolved.verse)
  )

  private def resolveToChapter(fromResolved: Verse): Int =
    to.chapter.getOrElse(fromResolved.chapter)
}

object SpanParsed {

  val parser: Parser[SpanParsed] = for {
    from <- VerseParsed.fromParser
    to <- VerseParsed.toParser
  } yield SpanParsed(from, to)
}
