package org.opentorah.texts.tanach

import org.opentorah.metadata.WithNumber
import org.opentorah.store.{By, Selector, Store, Stores}
import org.opentorah.xml.Parser

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

  def byChapter: By[Chapter] = byChapter(full)

  def byChapter(span: Span): By[Chapter] = Chapters.ByChapter(span, this)

object Chapters:

  final class ByChapter(span: Span, chapters: Chapters) extends By.Numbered[Chapter]("chapter"):
    override def minNumber: Int = span.from.chapter
    override def maxNumber: Int = span.to.chapter

    override protected def createNumberedStore(number: Int): Chapter = new Chapter(
      number,
      from = if number == minNumber then span.from.verse else 1,
      to =   if number == maxNumber then span.to.verse   else chapters.length(number)
    ):
      override def oneOf: Stores.Numbered[Chapter] = ByChapter.this

  class BySpan(selectorName: String, spans: Seq[Span], chapters: Chapters) extends By.Numbered[Store.Numbered](selectorName):
    override def minNumber: Int = 1
    override def length: Int = spans.length
    override protected def createNumberedStore(number: Int): Store.Numbered = ForSpan(number)

    private class ForSpan(override val number: Int) extends Store.Numbered, Store.Bys:
      override def oneOf: Stores.Numbered[Store.Numbered] = BySpan.this
      override def storesPure: Seq[By[?]] = Seq(chapters.byChapter(spans(number-1)))

  val parser: Parser[Chapters] = for
    chapters: Seq[WithNumber[Int]] <- Chapter.seq()
    _ <- WithNumber.checkConsecutive(chapters, "chapter")
  yield Chapters(WithNumber.dropNumbers(chapters))
