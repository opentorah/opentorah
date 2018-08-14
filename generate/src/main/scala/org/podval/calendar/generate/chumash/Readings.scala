package org.podval.calendar.generate.chumash

import org.podval.calendar.dates.Calendar
import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.calendar.jewish.SpecialDay
import Parsha._
import SpecialDay.{ShabbosBereshit, Pesach, Shavuot, TishaBav, shabbosAfter, shabbosBefore}

case class Readings(parsha: Parsha, secondParsha: Option[Parsha] = None) {
  def isCombined: Boolean = secondParsha.isDefined
}

/**
  * Determine weekly portion read on a given Shabbos.
  *
  */
/*
  Shulchan Aruch, Orach Chayim, Laws of New Month, 428:4.

  We always read "Tzav at Aharon" before Pesach in a non-leap year and Metzorah in a leap year
  except when Rosh Hashanah is on Thursday and
  the year is lacking and leap (Ramo: or full and leap) (Magen Avraham: thus, any leap year) [note 1]
  when we read "Acharei Mot" before Pesach;

  we always read "Bemidbar Sinai" before Shavuot;

  Tisha Be Av is before (we read) Vaetchanan;

  "Atem Nitzavim" (we read) before Rosh HaShanah;
  because of this, when Rosh HaShanah is on Monday or Tuesday,
  so that there are 2 Saturdays between Rosh HaShanah and Sukkot,
  we have to separate Nitzavim and Vayelech and to read Vayelech between
  Rosh HaShanah and Yom Kippur and Haazinu between Yom Kippur and Sukkot;
  mnemonics: King (Magen Avraham: Rosh HaShanah, when we say "The King of Judgement") on 2 or 3 - split Vayelech;
  but when Rosh HaShanah is Thursday or Saturday [note 2],
  there is only 1 Saturday between Rosh HaShanah and Sukkot,
  and we read Haazinu on it,
  then (we read) Vayelech together with Nitzavim before Rosh HaShanah.

  Mnemonics [connections with weekly portions - from Magen Avraham]:
    for non-leap year: commanded (Tzav, "command"), then Pesach;
    for leap year: segregated (Metzorah, segregation of a leper), then Pesach;
    [no mnemonics is given for the "exceptional" scenario that results in Acharei Mot being read before Pesach]

    get counted (Bemidbar, census), then stop (Shavuot, Atzeret - "stopping")

    fast (9th of Av), then pray (Vaetchanan, prayer)

    stand up (Nitzavim, "standing"), then sound (the Shofar, Rosh HaShanah)


  note 1
    Mechaber specifies leap year starting on Thursday that is "lacking";
    Ramo corrects to include "full" years too;
    Magen Avraham states that this means *any* leap year (that starts on Thursday).
    This is correct, since leap year that starts on Thursday is always either "lacking" or "full",
    never "regular" :)


  note 2
    The text specifies Friday ("vav") instead of Saturday ("zain");
    this is an obvious misprint: Mechaber states in 428:1 that Rosh Hashanah never falls on Friday!
    Mishnah Berurah (9) corrects it based on Tur and "old editions of Shulchan Aruch",
    but none of the commentators around the text do: probably, their text was still intact.
    Surprisingly, edition that I have, which was allegedly verified against "old editions",
    doesn't correct this misprint.


  My attempt to separate the requirements from the emergent phenomena and to spell out the obvious:

  Weekly portions are read in sequence, with no repetitions,
  on Saturdays that are not festivals or intermediate days.

  Cycle starts on Shabbat Bereshit - first Saturday after Simchat Torah.
  [Note: Simchat Torah is celebrated on the day after Shmini Atzeret outside of the Holy Land,
  but on Shmini Atzeret itself in the Holy Land; since Simchat Torah outside of the Holy Land
  never falls on Saturday (428:1 list days that Hoshanah Rabba, last intermediate day of Sukkot,
  can never fall on), first Saturday after the outside-of-the-Holy-Land Simchat Torah
  *is* Shabbat Bereshit everywhere.]

  Last weekly portion, VZotHaBerachah, is read on Simchat Torah, and thus is not included
  in the regular schedule.

  Requirements of the Shulchan Aruch uniquely define the schedule only when priorities of combining
  combinable portions are specified, which Shulchan Aruch itself doesn't do. Customs vary
  (see Magen Avraham 6); currently, those priorities are codified as follows:
    Tazria and Metzora cn be combined only if AchareiMot and Kedoshim are combined;
    AchareiMot and Kedoshim can be combined only if Behar and Bechukotai are combined;
    Chukat and Balak can be combined only if Matot and Masei are combined.

  When Shulchan Oruch says that Bemidbar is read before Shavuot, it uses the same word "before"
  as when it says that Tisha Be Av is before the reading of Vaetchanan and in other places in
  the paragraph - but the meaning is different: everywhere "before" means "immediately before",
  but here it means - "before and as close as possible, which can sometimes turn out a week before"
  (in which case, Naso - and not Bemidbar - is read on Shabbos immediately before Shavuot).

  Once reading for Shabbat after Tisha Be Av is fixed to be Vaetchanan, reading for Shabbat
  before next Rosh Ha Shanah is *guaranteed* to include Nitzavim (there is no adjustments that
  can be made anyway), so motivation the choice of Nitzavim for this reading brought by the
  commentators seems somewhat post-hoc (or serendipitous?).
  Criteria for combining Nitzavim and Vayelech are also satisfied automatically.

  The fact that requirement of the Shulchan Aruch are satisfied by the algorithm and the
  assumptions of the algorithm itself hold is verified by the unit tests for the years 1-6000;
  I am too lazy to prove the theorems :)
 */
object Readings {

  def readings(year: Year, inHolyLand: Boolean): Seq[(Day, Readings)] = {
    // Reading cycle goes into the next year, so we need to exclude the festivals for both years.
    val festivals: Seq[Day] =
      SpecialDay.festivals(inHolyLand).map(_(year)) ++
      SpecialDay.festivals(inHolyLand).map(_(year+1))

    // All Shabbos days that are not festivals from one Shabbos Bereshit until the next.
    val weeks: Seq[Day] =
      unfoldInfiniteSimple(ShabbosBereshit(year))(_ + Calendar.daysPerWeek)
      .takeWhile(_ < ShabbosBereshit(year+1))
      .filterNot(festivals.contains)
      .toList

    def weeksTo(day: Day): Int = weeks.takeWhile(_ < day).length

    val weeksToShavuot: Int = weeksTo(shabbosBefore(Shavuot(year)))

    val weeksFromShavuotToAfterTishaBeAv: Int = weeksTo(shabbosAfter(TishaBav(year))) - weeksToShavuot

    val combineBeforeBemidbarCandidate: Int = 33 - weeksToShavuot
    val combineBeforeVaetchananCandidate: Int = 11 - weeksFromShavuotToAfterTishaBeAv

    // When there are to many Saturdays before Shavuot to assign Bemidbar to the one immediately before Shavuot,
    // Bemidbar is read one week before Shavuot:
    val (combineVayakhelPekudei: Int, combineBeforeBemidbar: Int, combineBeforeVaetchanan: Int) =
      if (combineBeforeBemidbarCandidate < 0) (0, 0, combineBeforeBemidbarCandidate + combineBeforeVaetchananCandidate)
      else {
        val doCombineVayakhelPekudei: Boolean = (combineBeforeBemidbarCandidate == 4) || {
          // This tweak is required only for the Holy Land - and never for AchareiMot.
          val parshahBeforePesach: Int = weeksTo(shabbosBefore(Pesach(year))) + 1
          (parshahBeforePesach != 25) && (parshahBeforePesach != 28) // Tzav; Metzora
        }

        val combineVayakhelPekudei: Int = if (doCombineVayakhelPekudei) 1 else 0
        (combineVayakhelPekudei, combineBeforeBemidbarCandidate-combineVayakhelPekudei, combineBeforeVaetchananCandidate)
      }
    require(0 <= combineVayakhelPekudei && combineVayakhelPekudei <= 1)
    require(0 <= combineBeforeBemidbar && combineBeforeBemidbar <= 3)
    require(0 <= combineBeforeVaetchanan && combineBeforeVaetchanan <= 2)

    val weeksFromVaetchanan: Int = weeks.length - weeksToShavuot - weeksFromShavuotToAfterTishaBeAv
    val combineFromVaetchanan: Int = 9 - weeksFromVaetchanan
    require(0 <= combineFromVaetchanan && combineFromVaetchanan <= 1)

    // maximum: 33 portions; combine: 1+3
    val result = single(
      Bereshit, Noach, LechLecha, Vayeira, ChayeiSarah, Toledot,
      Vayetze, Vayishlach, Vayeshev, Miketz, Vayigash, Vayechi,
      Shemot, Vaeira, Bo, Beshalach, Yitro, Mishpatim,
      Terumah, Tetzaveh, KiTisa
    ) ++
    combineIf(combineVayakhelPekudei >= 1, Vayakhel, Pekudei) ++
    single(Vayikra, Tzav, Shemini) ++
    combineIf(combineBeforeBemidbar >= 3, Tazria, Metzora) ++
    combineIf(combineBeforeBemidbar >= 2, AchareiMot, Kedoshim) ++
    single(Emor) ++
    combineIf(combineBeforeBemidbar >= 1, Behar, Bechukotai) ++
    // maximum: 11 portions; combine: 2
    single(Bemidbar, Naso, Behaalotecha, Shlach, Korach) ++
    combineIf(combineBeforeVaetchanan >= 2, Chukat, Balak) ++
    single(Pinchas) ++
    combineIf(combineBeforeVaetchanan >= 1, Matot, Masei) ++
    single(Devarim) ++
    // maximum: 9 portions; combine: 1
    single(Vaetchanan, Eikev, Reeh, Shoftim, KiTeitzei, KiTavo) ++
    combineIf(combineFromVaetchanan >= 1, Nitzavim, Vayelech) ++
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

//  def unfold[A, B](start: A)(f: A => Option[(A, B)]): Stream[B] =
//    f(start).map { case (a, b) => b #:: unfold(a)(f) }.getOrElse(Stream.empty)
//
//  def unfoldInfinite[A, B](start: A)(f: A => (A, B)): Stream[B] =
//    f(start) match { case (a, b) => b #:: unfoldInfinite(a)(f) }

  def unfoldInfiniteSimple[A](start: A)(f: A => A): Stream[A] = start #:: unfoldInfiniteSimple(f(start))(f)
}
