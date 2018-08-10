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

  private abstract class Segment(val max: Int, combinable: Int) {
    final def min: Int = max - combinable

    final def isLengthAllowed(length: Int): Boolean = min <= length && length <= max

    final def zip(weeks: Seq[Day]): Seq[(Day, Readings)] = {
      val length = weeks.length

      require(isLengthAllowed(length), s"length $length is not between $min and $max")

      val readings = get(max - length)
      require(readings.length == length)

      weeks zip readings
    }

    protected def get(needToCombine: Int): Seq[Readings]

    protected final def single(parshas: Parsha*): Seq[Readings] = parshas.map(Readings(_))
  }

  def readings(year: Year, inHolyLand: Boolean): Seq[(Day, Readings)] = {
    val all = weeks(year, inHolyLand)

    //    Перед Шавуот читают Бемидбар
    //    Перед 9 ава или в сам 9 ава - Дварим
    val shabbosBeforeShavuot = SpecialDay.Shavuot(year).prev.prev(Day.Name.Shabbos)
    val shabbosBeforeOrOnTishaBeAv = SpecialDay.TishaBav(year).prev(Day.Name.Shabbos)

    beforeBemidbar.zip(all.takeWhile(_ < shabbosBeforeShavuot)) ++
    fromBemidbarUntilDevarim.zip(all.dropWhile(_ < shabbosBeforeShavuot).takeWhile(_ < shabbosBeforeOrOnTishaBeAv)) ++
    fromDevarim.zip(all.dropWhile(_ < shabbosBeforeOrOnTishaBeAv) )
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
    // Since the cycle goes into next year, we need to exclude the festivals and
    // intermediate days until next Simchat Torah.
    val exclude: Seq[Day] =
      SpecialDay.festivals(inHolyLand).map(_(year)) ++
      SpecialDay.festivals(inHolyLand).map(_(year+1))

    // TODO unfold flavour [A, A]
    val result = unfold(SpecialDay.ShabbosBereshit(year)) { shabbos => Some(shabbos + Calendar.daysPerWeek, shabbos) }
    .takeWhile(_ < SpecialDay.ShabbosBereshit(year+1))
    .filterNot(exclude.contains)
    .toList

    result
  }

  private object beforeBemidbar extends Segment(33, 4) {
    protected override def get(needToCombine: Int): Seq[Readings] = {
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
  }

  private object fromBemidbarUntilDevarim extends Segment(10, 2) {
    protected override def get(needToCombine: Int): Seq[Readings] = {
      single(Bemidbar, Naso, Behaalotecha, Shlach, Korach) ++
      combineIf(needToCombine >= 2, Chukat, Balak) ++
      single(Pinchas) ++
      combineIf(needToCombine >= 1, Matot, Masei)
    }
  }

  private object fromDevarim extends Segment(10, 1) {
    protected override def get(needToCombine: Int): Seq[Readings] = {
      single(Devarim, Vaetchanan, Eikev, Reeh, Shoftim, KiTeitzei, KiTavo) ++
      combineIf(needToCombine > 0, Nitzavim, Vayelech) ++
      single(Haazinu)
      // VZotHaBerachah is read on Simchat Torah, and thus is not included in the regular schedule
    }
  }

  private def combineIf(condition: Boolean, parsha: Parsha, secondParsha: Parsha): Seq[Readings] =
    if (condition) Seq(Readings(parsha, Some(secondParsha)))
    else Seq(Readings(parsha), Readings(secondParsha))

  // Will this *ever* be in the standard library?
  def unfold[A, B](start: A)(f: A => Option[(A, B)]): Stream[B] =
    f(start).map { case (a, b) => b #:: unfold(a)(f) }.getOrElse(Stream.empty)
}
