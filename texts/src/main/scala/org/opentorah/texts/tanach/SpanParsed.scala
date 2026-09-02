package org.opentorah.texts.tanach

import org.podval.xml.XmlAst

final class SpanParsed(val from: VerseParsed, val to: VerseParsed):

  def inheritFrom(ancestor: SpanParsed): SpanParsed = SpanParsed(
    from = this.from.inheritFrom(ancestor.from),
    to = this.to.inheritFrom(ancestor.to)
  )

  def defaultFromChapter(fromChapter: Int): SpanParsed =
    SpanParsed(from = from.defaultChapter(fromChapter), to = to)

  def semiResolve: SpanSemiResolved =
    val fromResolved = from.resolve
    SpanSemiResolved(fromResolved, semiResolveTo(fromResolved))

  private def semiResolveTo(fromResolved: ChapterAndVerse): Option[ChapterAndVerse] =
    require(to.verse.nonEmpty || to.chapter.isEmpty)

    if to.verse.isEmpty then None else Some(ChapterAndVerse(
      chapter = resolveToChapter(fromResolved),
      verse = to.verse.get
    ))

  def resolve: Span =
    val fromResolved = from.resolve
    Span(fromResolved, resolveTo(fromResolved))

  private def resolveTo(fromResolved: ChapterAndVerse): ChapterAndVerse = ChapterAndVerse(
    chapter = resolveToChapter(fromResolved),
    verse = to.verse.getOrElse(fromResolved.verse)
  )

  private def resolveToChapter(fromResolved: ChapterAndVerse): Int =
    to.chapter.getOrElse(fromResolved.chapter)

object SpanParsed:

  def decode[E: XmlAst](element: E): SpanParsed = SpanParsed(
    from = VerseParsed.decodeFrom(element),
    to = VerseParsed.decodeTo(element)
  )
