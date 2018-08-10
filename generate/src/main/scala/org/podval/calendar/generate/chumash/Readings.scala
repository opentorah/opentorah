package org.podval.calendar.generate.chumash

import org.podval.calendar.dates.Calendar
import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.calendar.jewish.SpecialDay
import Parsha._
import Day.Name.Shabbos

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

  private def single(parshas: Parsha*): Seq[Readings] = parshas.map(Readings(_))

  def readings(year: Year, inHolyLand: Boolean): Seq[(Day, Readings)] = {
    val all = weeks(year, inHolyLand)

    //    Перед Шавуот читают Бемидбар
    //    Перед 9 ава или в сам 9 ава - Дварим
    val shabbosBeforeShavuot = SpecialDay.Shavuot(year).prev.prev(Shabbos)
    val (beforeBemidbar, bemidbarAndAfter) = all.partition(_ < shabbosBeforeShavuot)
    val afterBemidbar = bemidbarAndAfter.tail
    val shabbosBeforeOrOnTishaBeAv = SpecialDay.TishaBav(year).prev(Shabbos)
    val (betweenBemidbarAndDevarim, devarimAndAfter) = afterBemidbar.partition(_ < shabbosBeforeOrOnTishaBeAv)
    val afterDevarim = devarimAndAfter.tail

    zip(beforeBemidbar, segmentBeforeBemidbar) ++
    Seq(shabbosBeforeShavuot -> Readings(Bemidbar)) ++
    zip(betweenBemidbarAndDevarim, segmentBetweenBemidbarAndDevarim) ++
    Seq(shabbosBeforeOrOnTishaBeAv -> Readings(Devarim)) ++
    zip(afterDevarim, segmentAfterDevarim)
  }

  private def zip(weeks: Seq[Day], segment: Int => Seq[Readings]): Seq[(Day, Readings)] = {
    val length: Int = weeks.length
    val readings: Seq[Readings] = segment(length)
    require(readings.length == length)
    weeks zip readings
  }

  /**
    * All Shabbos days for the cycle of the Torah reading starting this year during
    * which regular Parsha is read (i.e., excepting festivals and intermediate days).
    *
    * @param year  when the Torah reading cycle starts
    * @param inHolyLand  or in Diaspora
    * @return  Shabbos days when regular Parsha is read
    */
  def weeks(year: Year, inHolyLand: Boolean): Seq[Day] = {
    val exclude: Seq[Day] = SpecialDay.festivals(inHolyLand).map(_(year))

    // TODO unfold flavour [A, A]
    unfold(SpecialDay.ShabbosBereshit(year)) { shabbos => Some(shabbos + Calendar.daysPerWeek, shabbos) }
    .takeWhile(_ < SpecialDay.ShabbosBereshit(year+1))
    .filterNot(exclude.contains)
    .toList
  }

  private def segmentBeforeBemidbar(length: Int): Seq[Readings] = {
    // In this segment there are 33 portions and 4 combining pairs;
    // it can deliver lengths from 29 to 33.
    verifyLength(length, 29, 33)

    val needToCombine = 33-length
    val (combinePekudei, combineBeforeBemidbar) =
      if (needToCombine == 4) (true, 3)
      else (false, needToCombine)

    single(
      Bereshit, Noach, LechLecha, Vayeira, ChayeiSarah, Toledot,
      Vayetze, Vayishlach, Vayeshev, Miketz, Vayigash, Vayechi,
      Shemot, Vaeira, Bo, Beshalach, Yitro, Mishpatim,
      Terumah, Tetzaveh, KiTisa
    ) ++
    combineIf(combinePekudei, Vayakhel, Pekudei) ++
    single(Vayikra, Tzav, Shemini) ++
    combineIf(combineBeforeBemidbar >= 3, Tazria, Metzora) ++
    combineIf(combineBeforeBemidbar >= 2, AchareiMot, Kedoshim) ++
    single(Emor) ++
    combineIf(combineBeforeBemidbar >= 1, Behar, Bechukotai)
  }

  private def segmentBetweenBemidbarAndDevarim(length: Int): Seq[Readings] = {
    // In this segment there are 9 portions and 2 combining pairs;
    // it can deliver lengths from 7 to 9.
    verifyLength(length, 7, 9)

    val combineBeforeDevarim = 9-length

    single(Naso, Behaalotecha, Shlach, Korach) ++
    combineIf(combineBeforeDevarim >= 2, Chukat, Balak) ++
    single(Pinchas) ++
    combineIf(combineBeforeDevarim >= 1, Matot, Masei)
  }

  private def segmentAfterDevarim(length: Int): Seq[Readings] = {
    // In this segment there are 10 portions and 1 combining pairs;
    // it can deliver lengths from 9 to 10.
    verifyLength(length, 9, 10)

    val combineVayelech: Boolean = length < 10

    single(Vaetchanan, Eikev, Reeh, Shoftim, KiTeitzei, KiTavo) ++
    combineIf(combineVayelech, Nitzavim, Vayelech) ++
    single(Haazinu, VZotHaBerachah)
  }

  private def verifyLength(length: Int, from: Int, to: Int): Unit = {
    require(from <= length && length <= to, s"length $length is not between $from and $to")
  }

  private def combineIf(condition: Boolean, parsha: Parsha, secondParsha: Parsha): Seq[Readings] =
    if (condition) Seq(Readings(parsha, Some(secondParsha)))
    else Seq(Readings(parsha), Readings(secondParsha))



//  def beforePassover(year: Year): Parsha = if (!year.isLeap) Tzav else {
//    val roshHaShonohOnChamishi: Boolean = SpecialDay.RoshHashanah(year).name == Day.Name.Chamishi
//    val bothCheshvanAndKislevFullOrLacking = year.kind != Year.Kind.Regular
//    if (roshHaShonohOnChamishi && bothCheshvanAndKislevFullOrLacking) AchareiMot else Metzora
//  }

//  def beforeRoshHaShonoh(year: Year): Readings = {
//    val roshHaShonohDay: Day.Name = SpecialDay.RoshHashanah(year).name
//    val roshHaShonohOnSheniOrShlishi: Boolean =
//      (roshHaShonohDay == Day.Name.Sheni) || (roshHaShonohDay == Day.Name.Shlishi)
//    Readings(
//      parsha = Nitzavim,
//      secondParsha = if (roshHaShonohOnSheniOrShlishi) None else Some(Vayelech)
//    )
//  }

  def main(args: Array[String]): Unit = {
    val year = Year(5778)
//    weeks(year, inHolyLand = false).foreach(println)
    readings(year, inHolyLand = false).foreach(println)
  }

  // Will this *ever* be in the standard library?
  def unfold[A, B](start: A)(f: A => Option[(A, B)]): Stream[B] =
    f(start).map { case (a, b) => b #:: unfold(a)(f) }.getOrElse(Stream.empty)
}
