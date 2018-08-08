package org.podval.calendar.generate.chumash

import Parsha.{AchareiMot, Metzora, Tzav, Nitzavim, Vayelech}
import org.podval.calendar.jewish.Jewish.{Year, Month, Day}

case class Readings(parsha: Parsha, secondParsha: Option[Parsha])

/**
  * Determine weekly portion read on a given Shabbos.
  * Source of rules: Shulchan Aruch, Orach Chayim, Laws of New Month, Chapter 428.
  */
object Readings {

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
          parsha = Parsha.Vayakhel,
          secondParsha = if (combineVayakhelAndPekudei) Some(Parsha.Pekudei) else None
      ) else {
        val parsha: Parsha = number match {
          case  1 => Parsha.Bereshit
          case  2 => Parsha.Noach
          case  3 => Parsha.LechLecha
          case  4 => Parsha.Vayeira
          case  5 => Parsha.ChayeiSarah
          case  6 => Parsha.Toledot
          case  7 => Parsha.Vayetze
          case  8 => Parsha.Vayishlach
          case  9 => Parsha.Vayeshev
          case 10 => Parsha.Miketz
          case 11 => Parsha.Vayigash
          case 12 => Parsha.Vayechi
          case 13 => Parsha.Shemot
          case 14 => Parsha.Vaeira
          case 15 => Parsha.Bo
          case 16 => Parsha.Beshalach
          case 17 => Parsha.Yitro
          case 18 => Parsha.Mishpatim
          case 19 => Parsha.Terumah
          case 20 => Parsha.Tetzaveh
          case 21 => Parsha.KiTisa
          case 22 => Parsha.Vayakhel
          case 23 => Parsha.Pekudei
        }
        Readings(parsha = parsha, secondParsha = None)
      }
    } else

      ???

  }

  def roshHaShonoh(year: Year): Day = year.month(Month.Name.Tishrei).day(1)

  // TODO and in the Holy Land?
  def simchasTorah(year: Year): Day = year.month(Month.Name.Tishrei).day(23)

  def shabbosBereshit(year: Year): Day = shabbosAfter(simchasTorah(year))

  def passover(year: Year): Day = year.month(Month.Name.Nisan).day(15)

  def shavuot(year: Year): Day = year.month(Month.Name.Sivan).day(6)

  def tishaBeAv(year: Year): Day = year.month(Month.Name.Av).day(9)

  def shabbosBeforePassover(year: Year): Day = shabbosBefore(passover(year))

  def shabbosBeforeShavuot(year: Year): Day = shabbosBefore(shavuot(year))

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
    val beforePassover: Day = shabbosBeforePassover(year)
    println(beforePassover)
    println(forDay(beforePassover))
  }
}
