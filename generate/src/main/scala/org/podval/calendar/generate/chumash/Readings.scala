package org.podval.calendar.generate.chumash

import org.podval.calendar.dates.Calendar
import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.calendar.jewish.SpecialDay
import Parsha._

case class Readings(parsha: Parsha, secondParsha: Option[Parsha] = None) {
  def isCombined: Boolean = secondParsha.isDefined
}

/**
  * Determine weekly portion read on a given Shabbos.
  * Source: Shulchan Aruch, Orach Chayim, Laws of New Month, Chapter 428.
  *
  * verified: SimchasTorah doesn't fall on Shabbos, so Shabbos Breshit is the same here and there
  *
  */
object Readings {

  def readings(year: Year, inHolyLand: Boolean): Seq[(Day, Readings)] = {
    // Reading cycle goes into the next year, so we need to exclude the festivals for both years.
    val festivals: Seq[Day] =
      SpecialDay.festivals(inHolyLand).map(_(year)) ++
      SpecialDay.festivals(inHolyLand).map(_(year+1))

    // All Shabbos days that are not festivals from one Shabbos Bereshit until the next.
    val weeks: Seq[Day] =
      unfoldInfiniteSimple(SpecialDay.ShabbosBereshit(year))(_ + Calendar.daysPerWeek)
      .takeWhile(_ < SpecialDay.ShabbosBereshit(year+1))
      .filterNot(festivals.contains)
      .toList

    val shabbosBeforeShavuot = SpecialDay.Shavuot(year).prev.prev(Day.Name.Shabbos)
    val weeksBeforeShavuot: Int = weeks.takeWhile(_ < shabbosBeforeShavuot).length

    val shabbosBeforeOrOnTishaBeAv = SpecialDay.TishaBav(year).prev(Day.Name.Shabbos)
    val weeksFromShavuotToTishaBeAv: Int = weeks.takeWhile(_ < shabbosBeforeOrOnTishaBeAv).length - weeksBeforeShavuot

    val weeksAfterTishaBeAv: Int = weeks.length - weeksBeforeShavuot - weeksFromShavuotToTishaBeAv

    // Перед Шавуот читают Бемидбар, а если до Шавуота 34 недели - Насо TODO
    // Перед 9 ава или в сам 9 ава - Дварим
    val combineBeforeBemidbarCandidate: Int = 33 - weeksBeforeShavuot
    val combineBeforeDevarimCandidate: Int = 10 - weeksFromShavuotToTishaBeAv

    val (combineBeforeBemidbar: Int, combineBeforeDevarim: Int) =
      if (combineBeforeBemidbarCandidate >= 0) (combineBeforeBemidbarCandidate, combineBeforeDevarimCandidate)
      else (0, combineBeforeBemidbarCandidate + combineBeforeDevarimCandidate)
    require(0 <= combineBeforeBemidbar && combineBeforeBemidbar <= 4)
    require(0 <= combineBeforeDevarim && combineBeforeDevarim <= 2)

    val combineAfterDevarim: Int = 10 - weeksAfterTishaBeAv
    require(0 <= combineAfterDevarim && combineAfterDevarim <= 1)

    // maximum: 33 portions; combine: 4
    val result = single(
      Bereshit, Noach, LechLecha, Vayeira, ChayeiSarah, Toledot,
      Vayetze, Vayishlach, Vayeshev, Miketz, Vayigash, Vayechi,
      Shemot, Vaeira, Bo, Beshalach, Yitro, Mishpatim,
      Terumah, Tetzaveh, KiTisa
    ) ++
    combineIf(combineBeforeBemidbar >= 4, Vayakhel, Pekudei) ++
    single(Vayikra, Tzav, Shemini) ++
    combineIf(combineBeforeBemidbar >= 3, Tazria, Metzora) ++
    combineIf(combineBeforeBemidbar >= 2, AchareiMot, Kedoshim) ++
    single(Emor) ++
    combineIf(combineBeforeBemidbar >= 1, Behar, Bechukotai) ++
    // maximum: 10 portions; combine: 2
    single(Bemidbar, Naso, Behaalotecha, Shlach, Korach) ++
    combineIf(combineBeforeDevarim >= 2, Chukat, Balak) ++
    single(Pinchas) ++
    combineIf(combineBeforeDevarim >= 1, Matot, Masei) ++
    // maximum: 10 portions; combine: 1
    single(Devarim, Vaetchanan, Eikev, Reeh, Shoftim, KiTeitzei, KiTavo) ++
    combineIf(combineAfterDevarim >= 1, Nitzavim, Vayelech) ++
    single(Haazinu)
    // VZotHaBerachah is read on Simchat Torah, and thus is not included in the regular schedule

    require(result.length == weeks.length)
    weeks zip result
  }

  private def single(parshas: Parsha*): Seq[Readings] = parshas.map(Readings(_))

  private def combineIf(condition: Boolean, parsha: Parsha, secondParsha: Parsha): Seq[Readings] =
    if (condition) Seq(Readings(parsha, Some(secondParsha)))
    else Seq(Readings(parsha), Readings(secondParsha))

  // Will this *ever* be in the standard library?

  def unfold[A, B](start: A)(f: A => Option[(A, B)]): Stream[B] =
    f(start).map { case (a, b) => b #:: unfold(a)(f) }.getOrElse(Stream.empty)

  def unfoldInfinite[A, B](start: A)(f: A => (A, B)): Stream[B] =
    f(start) match { case (a, b) => b #:: unfoldInfinite(a)(f) }

  def unfoldInfiniteSimple[A](start: A)(f: A => A): Stream[A] =
    start #:: unfoldInfiniteSimple(f(start))(f)
}
