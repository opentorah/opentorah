package org.podval.calendar.generate.chumash

import org.podval.calendar.dates.Calendar
import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.calendar.jewish.SpecialDay
import Parsha._

case class Readings(parsha: Parsha, secondParsha: Option[Parsha] = None)

/**
  * Determine weekly portion read on a given Shabbos.
  * Source: Shulchan Aruch, Orach Chayim, Laws of New Month, Chapter 428.
  *
  * verified: SimchasTorah doesn't fall on Shabbos, so Shabbos Breshit is the same here and there
  *
  *
  */
object Readings {

  def readings
  (
    combinePekudei: Boolean,
    combineBeforeBamidbar: Int,
    combineBeforeDvarim: Int,
    combineVayelech: Boolean
  ): Seq[Readings] = {
    require(0 <= combineBeforeBamidbar && combineBeforeBamidbar <= 3)
    require(0 <= combineBeforeDvarim && combineBeforeDvarim <= 2)

    def single(parshas: Parsha*): Seq[Readings] = parshas.map(Readings(_))

    single(
      Bereshit, Noach, LechLecha, Vayeira, ChayeiSarah, Toledot, Vayetze, Vayishlach, Vayeshev, Miketz, Vayigash, Vayechi,
      Shemot, Vaeira, Bo, Beshalach, Yitro, Mishpatim, Terumah, Tetzaveh, KiTisa
    ) ++
    combineIf(combinePekudei, Vayakhel, Pekudei) ++
    single(Vayikra, Tzav, Shemini) ++
    combineIf(combineBeforeBamidbar >= 3, Tazria, Metzora) ++
    combineIf(combineBeforeBamidbar >= 2, AchareiMot, Kedoshim) ++
    single(Emor) ++
    combineIf(combineBeforeBamidbar >= 1, Behar, Bechukotai) ++
    single(Bemidbar, Naso, Behaalotecha, Shlach, Korach) ++
    combineIf(combineBeforeDvarim >= 2, Chukat, Balak) ++
    single(Pinchas) ++
    combineIf(combineBeforeDvarim >= 1, Matot, Masei) ++
    single(Devarim, Vaetchanan, Eikev, Reeh, Shoftim, KiTeitzei, KiTavo) ++
    combineIf(combineVayelech, Nitzavim, Vayelech) ++
    single(Haazinu, VZotHaBerachah)
  }


  def combineIf(condition: Boolean, parsha: Parsha, secondParsha: Parsha): Seq[Readings] =
    if (condition) Seq(Readings(parsha, Some(secondParsha)))
    else Seq(Readings(parsha), Readings(secondParsha))


  def nextShabbos(day: Day): Day = day.next(Day.Name.Shabbos)

  /**
    * All Shabbos days for the cycle of the Torah reading starting this year during
    * which regular Parsha is read (i.e., excepting festivals and intermediate days).
    *
    * @param year  when the Torah reading cycle starts
    * @param inHolyLand  or in Diaspora
    * @return  Shabbos days when regular Parsha is read
    */
  def shabbotim(year: Year, inHolyLand: Boolean): Seq[Day] = {
    val from: Day = SpecialDay.ShabbosBereshit(year)
    val until: Day = SpecialDay.ShabbosBereshit(year+1)
    val all: Seq[Day] = unfold(from) { shabbos =>
      val next = shabbos + Calendar.daysPerWeek
      if (next >= until) None else Some(next, shabbos)
    }

    val exclude: Seq[Day] = SpecialDay.festivals(inHolyLand).map(_(year))
    all.toList.filterNot(exclude.contains)
  }


  def beforePassover(year: Year): Parsha = if (!year.isLeap) Tzav else {
    val roshHaShonohOnChamishi: Boolean = SpecialDay.RoshHashanah(year).name == Day.Name.Chamishi
    val bothCheshvanAndKislevFullOrLacking = year.kind != Year.Kind.Regular
    if (roshHaShonohOnChamishi && bothCheshvanAndKislevFullOrLacking) AchareiMot else Metzora
  }

  /*
  Перед Шавуот читают Бемидбар

  Перед 9 ава или в сам 9 ава - Дварим
   */

  def beforeRoshHaShonoh(year: Year): Readings = {
    val roshHaShonohDay: Day.Name = SpecialDay.RoshHashanah(year).name
    val roshHaShonohOnSheniOrShlishi: Boolean =
      (roshHaShonohDay == Day.Name.Sheni) || (roshHaShonohDay == Day.Name.Shlishi)
    Readings(
      parsha = Nitzavim,
      secondParsha = if (roshHaShonohOnSheniOrShlishi) None else Some(Vayelech)
    )
  }

  def main(args: Array[String]): Unit = {
    val year = Year(5778)
    shabbotim(year, inHolyLand = false).foreach(println)

//    val beforePassover: Day = shabbosBeforePassover(year)
//    println(beforePassover)
//    println(forDay(beforePassover))
  }

  // Will this *ever* be in the standard library?
  def unfold[A, B](start: A)(f: A => Option[(A, B)]): Stream[B] =
    f(start).map { case (a, b) => b #:: unfold(a)(f) }.getOrElse(Stream.empty)
}
