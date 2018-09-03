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

  def validate(chapter: Int, verse: Int, chapters: Array[Int]): Unit = {
    if (chapter <= 0) throw new IllegalArgumentException("Non-positive chapter.")
    if (verse <= 0) throw new IllegalArgumentException("Non-positive verse.")
    if (chapter > chapters.length) throw new IllegalArgumentException("Chapter out of range")
    if (verse > chapters(chapter-1)) throw new IllegalArgumentException("Verse out of range")
  }

  def prev(chapter: Int, verse: Int, chapters: Array[Int]): (Int, Int) = {
    validate(chapter, verse, chapters)
    if (verse > 1) (chapter, verse-1)
    else if (chapter > 1) (chapter-1, chapters(chapter-2))
    else throw new IllegalArgumentException("No chapters before the first one.")
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
  // TODO validate week/aliyah/day/maftir against chapters!
  def apply(
    chaptersParsed: Seq[ChapterParsed],
    weeksParsed: Seq[WeekParsed]
  ): ChumashBook = {
    val chapters = TanachBook.checkChapters(chaptersParsed)

    // Validate 'from' for each week.
    weeksParsed.foreach(week => TanachBook.validate(week.fromChapter, week.fromVerse, chapters))

    // Set implied toChapter/toVerse on weeks.
    val weeksWithTo =
      weeksParsed.zip(weeksParsed.tail).map { case (week, nextWeek) =>
        val (toChapterImplied, toVerseImplied) = TanachBook.prev(nextWeek.fromChapter, nextWeek.fromVerse, chapters)
        setImpliedTo(week, toChapterImplied, toVerseImplied)
      } :+ setImpliedTo(weeksParsed.last, chapters.length, chapters(chapters.length-1))

    val weeks: Seq[Week] = weeksWithTo.map { week =>
      val maftir = Week.Maftir(
        fromChapter = week.maftir.fromChapter,
        fromVerse = week.maftir.fromVerse
      )
      Week(
        week.names,
        week.fromChapter,
        week.fromVerse,
        maftir
      )
    }

    new ChumashBook(
      weeks.head.names,
      chapters,
      weeks
    )
  }

  private def setImpliedTo(week: WeekParsed, toChapterImplied: Int, toVerseImplied: Int): WeekParsed = {
    if (week.toChapter.nonEmpty && !week.toChapter.contains(toChapterImplied))
      throw new IllegalArgumentException("Wrong explicit 'toChapter'")
    if (week.toVerse.nonEmpty && !week.toVerse.contains(toVerseImplied))
      throw new IllegalArgumentException("Wrong explicit 'toVerse'")
    week.copy(toChapter = Some(toChapterImplied), toVerse = Some(toVerseImplied))
  }
}

final case class ChapterParsed(n: Int, length: Int)


final case class WeekParsed(
  names: Names,
  fromChapter: Int,
  fromVerse: Int,
  toChapter: Option[Int],
  toVerse: Option[Int],
  days: Seq[WeekParsed.Day],
  aliyot: Seq[WeekParsed.Aliyah],
  maftir: WeekParsed.Maftir
)

object WeekParsed {
  final case class Aliyah(
    n: Int,
    fromChapter: Int,
    fromVerse: Int,
    toChapter: Option[Int],
    toVerse: Option[Int]
  )

  final case class Day(
    n: Int,
    fromChapter: Int,
    fromVerse: Int,
    toChapter: Option[Int],
    toVerse: Option[Int],
    custom: Option[String],
    isCombined: Boolean
  )

  final case class Maftir(
    fromChapter: Int,
    fromVerse: Int,
    toChapter: Option[Int],
    toVerse: Option[Int]
  )
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

object Main {
  def main(args: Array[String]): Unit = {
    val result = TanachParser.parse
    val z = 0
  }
}
