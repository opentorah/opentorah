package org.opentorah.texts.tanach

import org.podval.store.{By, NumberedStore, NumberedStores, Stores}

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

  private def axis(span: Span): By[Chapter] = By.numbered("chapter", span.from.chapter, span.to.chapter):
    (number, parent) =>
      new Chapter(
        number,
        from = if number == span.from.chapter then span.from.verse else 1,
        to = if number == span.to.chapter then span.to.verse else this.length(number)
      ):
        override def oneOf: NumberedStores[Chapter] = parent

object Chapters:
  class BySpan(selectorName: String, spans: Seq[Span], chapters: Chapters) extends By.Numbered[NumberedStore](selectorName):
    override def minNumber: Int = 1
    override def length: Int = spans.length
    override protected def createNumberedStore(number: Int): NumberedStore = ForSpan(number)

    private class ForSpan(override val number: Int) extends NumberedStore with Stores[?]:
      override def oneOf: NumberedStores[NumberedStore] = BySpan.this
      private lazy val byChapter: By[Chapter] = chapters.byChapter(spans(number - 1))
      override def stores: Seq[By[?]] = Seq(byChapter)
