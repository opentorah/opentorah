package org.podval.calendar.generate.tanach

// TODO introduce Tanach and bind from Parsha
abstract class TanachBook(
  names: Names,
  chapters: Array[Int]
)

object TanachBook {
  def checkChapters(chapters: Seq[ChapterParsed]): Array[Int] = {
    if (chapters.map(_.n) != (1 to chapters.length)) throw new IllegalArgumentException("Wrong chapter numbers.")
    chapters.map(_.length).toArray
  }
}

final case class ChumashBook private(
  names: Names,
  chapters: Array[Int],
  weeks: Seq[Week]
) extends TanachBook(names, chapters)

final case class Week private(
  names: Names,
  fromChapter: Int,
  fromVerse: Int,
// TODO calculate
//  toChapter: Int,
//  toVerse: Int,
  maftir: Week.Maftir
) {
  if (fromChapter <= 0) throw new IllegalArgumentException("Non-positive 'fromChapter'.")
  if (fromVerse <= 0) throw new IllegalArgumentException("Non-positive 'fromVerse'.")
}

object Week {
  final case class Aliyah(fromChapter: Int, fromVerse: Int, toChapter: Int, toVerse: Int)

  // TODO add custom and combined
  // TODO check against Parsha what can be combined
  final case class Day(fromChapter: Int, fromVerse: Int, toChapter: Int, toVerse: Int)

  final case class Maftir(fromChapter: Int, fromVerse: Int)
}

object ChumashBook {
  // TODO check that chapters are numbered correctly
  // TODO validate week/aliyah/day/maftir against chapters!
  def apply(
    chaptersParsed: Seq[ChapterParsed],
    weeksParsed: Seq[WeekParsed]
  ): ChumashBook = {
    val chapters = TanachBook.checkChapters(chaptersParsed)
    val weeks: Seq[Week] = weeksParsed.map { week =>
      Week(
        week.names,
        week.fromChapter,
        week.fromVerse,
        week.maftir
      )
    }

    new ChumashBook(
      weeks.head.names,
      chapters,
      weeks
    )
  }
}

final case class ChapterParsed(n: Int, length: Int)


final case class WeekParsed(
  names: Names,
  fromChapter: Int,
  fromVerse: Int,
  days: Seq[WeekParsed.Day],
  aliyot: Seq[WeekParsed.Aliyah],
  maftir: Week.Maftir
)

object WeekParsed {
  final case class Aliyah(n: Int, fromChapter: Int, fromVerse: Int, toChapter: Option[Int], toVerse: Option[Int])
  final case class Day(n: Int, fromChapter: Int, fromVerse: Int, toChapter: Option[Int], toVerse: Option[Int])
}

final class NachBook private(
  names: Names,
  chapters: Array[Int]
) extends TanachBook(names, chapters)

object NachBook {
  def apply(
    names: Names,
    chaptersParsed: Seq[ChapterParsed]
  ): NachBook = new NachBook(names, TanachBook.checkChapters(chaptersParsed))
}
