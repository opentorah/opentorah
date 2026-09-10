package org.opentorah.texts.tanach

import org.podval.metadata.Names
import org.podval.store.{By, NumberedStore, NumberedStores, Selectors, Stores}

final class Chapters(chapters: Seq[Int]):
  def length(chapter: Int): Int = chapters(chapter-1)

  def next(chapterAndVerse: ChapterAndVerse): Option[ChapterAndVerse] =
    require(contains(chapterAndVerse))
    if chapterAndVerse.verse < length(chapterAndVerse.chapter) then
      Some(ChapterAndVerse(chapterAndVerse.chapter, chapterAndVerse.verse+1))
    else if chapterAndVerse.chapter+1 <= chapters.length then
      Some(ChapterAndVerse(chapterAndVerse.chapter+1, 1))
    else
      None

  def prev(chapterAndVerse: ChapterAndVerse): Option[ChapterAndVerse] =
    require(contains(chapterAndVerse))
    if chapterAndVerse.verse > 1 then
      Some(ChapterAndVerse(chapterAndVerse.chapter, chapterAndVerse.verse-1))
    else if chapterAndVerse.chapter-1 >= 1 then
      Some(ChapterAndVerse(chapterAndVerse.chapter-1, length(chapterAndVerse.chapter-1)))
    else
      None

  def first: ChapterAndVerse = ChapterAndVerse(1, 1)

  def last: ChapterAndVerse = ChapterAndVerse(chapters.length, length(chapters.length))

  def full: Span = Span(first, last)

  def contains(span: Span): Boolean = contains(span.from) && contains(span.to)

  def contains(chapterAndVerse: ChapterAndVerse): Boolean =
    (chapterAndVerse.chapter <= chapters.length) && (chapterAndVerse.verse <= length(chapterAndVerse.chapter))

  def consecutive(first: Span, second: Span): Boolean =
    require(contains(first))
    require(contains(second))
    val nextVerse = next(first.to)
    nextVerse.fold(false)(_ == second.from)

  def consecutive(spans: Seq[Span]): Boolean =
    spans.zip(spans.tail).forall((first, second) => consecutive(first, second))

  def merge(first: Span, second: Span): Span =
    require(consecutive(first, second))
    Span(first.from, second.to)

  def cover(spans: Seq[Span], span: Span): Boolean =
    require(contains(span))
    consecutive(spans) && (spans.head.from == span.from) && (spans.last.to == span.to)

  lazy val byChapter: By[Chapter] = axis(full)

  def byChapter(span: Span): By[Chapter] = if span == full then byChapter else axis(span)

  private def axis(span: Span): By[Chapter] = By.Numbered("chapter", span.from.chapter, span.to.chapter):
    (number, parent) =>
      Chapter(
        number,
        from = if number == span.from.chapter then span.from.verse else 1,
        to = if number == span.to.chapter then span.to.verse else this.length(number),
        parent
      )

object Chapters:
  class BySpan(
    selectorName: String,
    spans: Seq[Span],
    chapters: Chapters,
    fromName: String => Option[Int] = NumberedStores.parseNumber,
    toNames: Int => Names = NumberedStores.namesForNumber
  )(using Selectors) extends By.Numbered[NumberedStore](
    summon[Selectors].getForName(selectorName),
    fromName,
    toNames
  ):
    override def minNumber: Int = 1
    override def length: Int = spans.length
    override protected def createNumberedStore(number: Int): NumberedStore = ForSpan(number, this)

    private class ForSpan(
      override val number: Int,
      override val oneOf: NumberedStores[NumberedStore]
    ) extends NumberedStore with Stores[?]:
      private lazy val byChapter: By[Chapter] = chapters.byChapter(spans(number - 1))
      override def stores: Seq[By[?]] = Seq(byChapter)
