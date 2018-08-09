package org.podval.calendar.generate.chumash

import org.podval.calendar.dates.Calendar
import org.podval.calendar.jewish.Jewish.{Day, Month, Year}
import org.podval.calendar.jewish.SpecialDay
import Parsha._

case class Readings(parsha: Parsha, secondParsha: Option[Parsha] = None)

/**
  * Determine weekly portion read on a given Shabbos.
  * Source of rules: Shulchan Aruch, Orach Chayim, Laws of New Month, Chapter 428.
  */
object Readings {

  val fromBereishitToKiTisa: Seq[Readings] = Seq(
    Bereshit, Noach, LechLecha, Vayeira, ChayeiSarah, Toledot, Vayetze, Vayishlach, Vayeshev, Miketz, Vayigash, Vayechi,
    Shemot, Vaeira, Bo, Beshalach, Yitro, Mishpatim, Terumah, Tetzaveh, KiTisa
  ).map(Readings(_))

  // Will this ever be in the standard library?
  def unfold[A, B](start: A)(f: A => Option[(A, B)]): Stream[B] =
    f(start).map { case (a, b) => b #:: unfold(a)(f) }.getOrElse(Stream.empty)

  def allShabbotim(year: Year): Seq[Day] = unfold(year.firstDay.next(Day.Name.Shabbos)) { shabbos =>
    val next = shabbos + Calendar.daysPerWeek
    if (next.year != year) None else Some(next, shabbos)
  }.toList

  def daysWhenRegularParshaIsNotRead(year: Year, inHolyLand: Boolean): Seq[Day] = Seq(
    SpecialDay.RoshHashanah,
    SpecialDay.YomKippur,
    SpecialDay.Sukkot,
    SpecialDay.Pesach,
    SpecialDay.Shavuot
  ).flatMap(_.days(year, inHolyLand))

  // TODO drop until Shabbos Breshit from which we start
  def shabbotimWhenRegularParshaIsRead(year: Year, inHolyLand: Boolean): Seq[Day] = {
    val exclude: Seq[Day] = daysWhenRegularParshaIsNotRead(year, inHolyLand)
    allShabbotim(year).filterNot(exclude.contains)
  }

  def forDay(day: Day): Readings = {
    val year = day.year
    val shabbos = day.next(Day.Name.Shabbos)

    if (shabbos < shabbosBereshit(year)) {
      throw new UnsupportedOperationException("Koritz didn't specify")
    } else

    // Shabbos Breshit to Passover
    if (shabbos <= shabbosBeforePassover(year)) {
      val betweenSimchasTorahAndPassover: Int =
        numberOfShabbosAfterShabbos(shabbosBeforePassover(year), shabbosBereshit(year))
      assert((betweenSimchasTorahAndPassover == 22) || (betweenSimchasTorahAndPassover == 23))
      val combineVayakhelAndPekudei: Boolean = betweenSimchasTorahAndPassover == 22

      val number: Int = numberOfShabbosAfterShabbos(shabbos, shabbosBereshit(year))

      if (combineVayakhelAndPekudei && (number == 22)) Readings(
          parsha = Vayakhel,
          secondParsha = if (combineVayakhelAndPekudei) Some(Pekudei) else None
      ) else {
        val parsha: Parsha = number match {
          case  1 => Bereshit
          case  2 => Noach
          case  3 => LechLecha
          case  4 => Vayeira
          case  5 => ChayeiSarah
          case  6 => Toledot
          case  7 => Vayetze
          case  8 => Vayishlach
          case  9 => Vayeshev
          case 10 => Miketz
          case 11 => Vayigash
          case 12 => Vayechi
          case 13 => Shemot
          case 14 => Vaeira
          case 15 => Bo
          case 16 => Beshalach
          case 17 => Yitro
          case 18 => Mishpatim
          case 19 => Terumah
          case 20 => Tetzaveh
          case 21 => KiTisa
          case 22 => Vayakhel
          case 23 => Pekudei
        }
        Readings(parsha = parsha)
      }
    } else

      ???

  }

  def roshHaShonoh(year: Year): Day = year.month(Month.Name.Tishrei).day(1)

  // TODO and in the Holy Land?
  def simchasTorah(year: Year): Day = year.month(Month.Name.Tishrei).day(23)

  def shabbosBereshit(year: Year): Day = shabbosAfter(simchasTorah(year))

  def shabbosBeforePassover(year: Year): Day = shabbosBefore(SpecialDay.Pesach.start(year))

  def shabbosBeforeShavuot(year: Year): Day = shabbosBefore(SpecialDay.Shavuot.start(year))

  def shabbosBefore(day: Day): Day = day.prev.prev(Day.Name.Shabbos)

  def shabbosAfter(day: Day): Day = day.next.next(Day.Name.Shabbos)

  def numberOfShabbosAfterShabbos(shabbos: Day, after: Day): Int = {
    if (shabbos.name != Day.Name.Shabbos) throw new IllegalArgumentException(s"$shabbos is not a Shabbos")
    if (after.name != Day.Name.Shabbos) throw new IllegalArgumentException(s"$after is not a Shabbos")
    if (shabbos < after) throw new IllegalArgumentException(s"$shabbos is before $after")
    if (shabbos == after) 0 else 1 + numberOfShabbosAfterShabbos(shabbosBefore(shabbos), after)
  }

  def beforePassover(year: Year): Parsha = if (!year.isLeap) Tzav else {
    val roshHaShonohOnChamishi: Boolean = roshHaShonoh(year).name == Day.Name.Chamishi
    val bothCheshvanAndKislevFullOrLacking = year.kind != Year.Kind.Regular
    if (roshHaShonohOnChamishi && bothCheshvanAndKislevFullOrLacking) AchareiMot else Metzora
  }

  /*
  Перед Шавуот читают Бемидбар

  Перед 9 ава или в сам 9 ава - Дварим
   */

  def beforeRoshHaShonoh(year: Year): Readings = {
    val roshHaShonohDay: Day.Name = roshHaShonoh(year).name
    val roshHaShonohOnSheniOrShlishi: Boolean =
      (roshHaShonohDay == Day.Name.Sheni) || (roshHaShonohDay == Day.Name.Shlishi)
    Readings(
      parsha = Nitzavim,
      secondParsha = if (roshHaShonohOnSheniOrShlishi) None else Some(Vayelech)
    )
  }

  def main(args: Array[String]): Unit = {
    val year = Year(5778)
    shabbotimWhenRegularParshaIsRead(year, false).foreach(println)

//    val beforePassover: Day = shabbosBeforePassover(year)
//    println(beforePassover)
//    println(forDay(beforePassover))
  }
}
