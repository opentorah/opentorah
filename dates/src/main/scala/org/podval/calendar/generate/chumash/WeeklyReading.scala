package org.podval.calendar.generate.chumash

import org.podval.calendar.dates.Calendar
import org.podval.calendar.jewish.Jewish.{Day, Year}
import org.podval.calendar.jewish.SpecialDay
import Parsha._
import SpecialDay.{ShabbosBereshit, Pesach, Shavuot, TishaBav, shabbosAfter, shabbosBefore}

/**
  * Determine weekly portion read on a given Shabbos.
  *
  */
/*
  Shulchan Aruch, Orach Chayim, Laws of New Month, 428:4.

  We always read "Tzav es Aharon" before Pesach in a non-leap year and Metzora in a leap year
  except when Rosh Hashanah is on Thursday and
  the year is lacking and leap (Ramo: or full and leap) (Magen Avraham: thus, any leap year) [note 1]
  when we read "Acharei Mos" before Pesach;

  we always read "Bemidbar Sinai" before Shavuot;

  Tisha Be Av is before (we read) Va'eschanan;

  "Atem Nitzavim" (we read) before Rosh HaShanah;
  because of this, when Rosh HaShanah is on Monday or Tuesday,
  so that there are 2 Saturdays between Rosh HaShanah and Sukkot,
  we have to separate Nitzavim and Vayeilech and to read Vayeilech between
  Rosh HaShanah and Yom Kippur and Haazinu between Yom Kippur and Sukkot;
  mnemonics: King (Magen Avraham: Rosh HaShanah, when we say "The King of Judgement") on 2 or 3 - split Vayeilech;
  but when Rosh HaShanah is Thursday or Saturday [note 2],
  there is only 1 Saturday between Rosh HaShanah and Sukkot,
  and we read Haazinu on it,
  then (we read) Vayeilech together with Nitzavim before Rosh HaShanah.

  Mnemonics [connections with weekly portions - from Magen Avraham]:
    for non-leap year: commanded (Tzav, "command"), then Pesach;
    for leap year: segregated (Metzora, segregation of a leper), then Pesach;
    [no mnemonics is given for the "exceptional" scenario that results in Acharei Mos being read before Pesach]

    get counted (Bemidbar, census), then stop (Shavuot, Atzeret - "stopping")

    fast (9th of Av), then pray (Va'eschanan, prayer)

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

  Cycle starts on Shabbat Bereishis - first Saturday after Simchat Torah.
  [Note: Simchat Torah is celebrated on the day after Shmini Atzeret outside of the Holy Land,
  but on Shmini Atzeret itself in the Holy Land; since Simchat Torah outside of the Holy Land
  never falls on Saturday (428:1 list days that Hoshanah Rabba, last intermediate day of Sukkot,
  can never fall on), first Saturday after the outside-of-the-Holy-Land Simchat Torah
  *is* Shabbat Bereshit everywhere.]

  Last weekly portion, Vezos Haberacha, is read on Simchat Torah, and thus is not included
  in the regular schedule.

  Requirements of the Shulchan Aruch uniquely define the schedule only when priorities of combining
  combinable portions are specified, which Shulchan Aruch itself doesn't do. Customs vary
  (see Magen Avraham 6); currently, those priorities are codified as follows:
    Behar and Bechukosai can be combined only if Acharei and Kedoshim are combined;
    Acharei and Kedoshim can be combined only if Tazria and Metzora are combined;
    Chukas and Balak can be combined only if Mattos and Masei are combined.
  TODO?
  Source: https://orot.ac.il/sites/default/files/morashtenu/16-3.pdf

  When Shulchan Oruch says that Bemidbar is read before Shavuot, it uses the same word "before"
  as when it says that Tisha Be Av is before the reading of Va'eschanan and in other places in
  the paragraph - but the meaning is different: everywhere "before" means "immediately before",
  but here it means - "before and as close as possible, which can sometimes turn out a week before"
  (in which case, Nasso - and not Bemidbar - is read on Shabbos immediately before Shavuot).

  Once reading for Shabbat after Tisha Be Av is fixed to be Va'eschanan, reading for Shabbat
  before next Rosh Ha Shanah is *guaranteed* to include Nitzavim (there is no adjustments that
  can be made anyway), so motivation the choice of Nitzavim for this reading brought by the
  commentators seems somewhat post-hoc (or serendipitous?).
  Criteria for combining Nitzavim and Vayeilech are also satisfied automatically.

  The fact that requirement of the Shulchan Aruch are satisfied by the algorithm and the
  assumptions of the algorithm itself hold is verified by the unit tests for the years 1-6000;
  I am too lazy to prove the theorems :)
 */
final case class WeeklyReading(parsha: Parsha, secondParsha: Option[Parsha]) {
  def isCombined: Boolean = secondParsha.isDefined
}

object WeeklyReading {
  private final val fromBereishisToBemidbar: Int = Parsha.distance(Bereishis, Bemidbar)
  private final val combinableFromBereishisToVayikra: Seq[Parsha] = Seq(Vayakhel)
  private final val allowedBeforePesach: Set[Parsha] = Set[Parsha](Tzav, Metzora, Acharei)
  // TODO see #56; Magen Avraham 428:4 (6);
  // Reversing the priorities here currently affects only non-leap regular years with Rosh
  // Hashanah on Thursday (and Pesach on Shabbat).
  private final val combinableFromVayikraToBemidbar: Seq[Parsha] = Seq(Tazria, Acharei, Behar)
  private final val fromBemidbarToVa_eschanan: Int = Parsha.distance(Bemidbar, Va_eschanan)
  private final val combinableFromBemidbarToVa_eschanan: Seq[Parsha] = Seq(Mattos, Chukas)
  private final val fromVa_eschanan: Int = Parsha.distance(Va_eschanan, VezosHaberacha)
  private final val combinableFromVa_eschanan: Seq[Parsha] = Seq(Nitzavim)

  def getYear(year: Year, inHolyLand: Boolean): Map[Day, WeeklyReading] =
    (getCycle(year-1, inHolyLand) ++ getCycle(year, inHolyLand)).filter(_._1.year == year).toMap

  def getCycle(year: Year, inHolyLand: Boolean): Seq[(Day, WeeklyReading)] = {
    // Reading cycle goes into the next year, so we need to exclude the festivals for both years.
    val festivals: Set[Day] =
      SpecialDay.festivals(inHolyLand).map(_(year)) ++
      SpecialDay.festivals(inHolyLand).map(_(year+1))

    // All Shabbos days that are not festivals from one Shabbos Bereshit until the next.
    val weeks: Seq[Day] =
      Util.unfoldInfiniteSimple(ShabbosBereshit(year))(_ + Calendar.daysPerWeek)
      .takeWhile(_ < ShabbosBereshit(year+1))
      .filterNot(festivals.contains)
      .toList

    def weeksTo(day: Day): Int = weeks.takeWhile(_ < day).length

    val weeksToShavuot: Int = weeksTo(shabbosBefore(Shavuot(year)))

    val weeksFromShavuotToAfterTishaBeAv: Int = weeksTo(shabbosAfter(TishaBav(year))) - weeksToShavuot

    // When there are to many Saturdays before Shavuot to assign Bemidbar to the one immediately before Shavuot,
    // Bemidbar is read one week before Shavuot:
    val (combineFromBereishisToVayikra: Int, combineFromVayikraToBemidbar: Int, combineFromBemidbarToVa_eschanan: Int) = {
      val combinefromBereishisToBemidbar: Int = fromBereishisToBemidbar - weeksToShavuot
      val combineFromBemidbarToVa_eschananCandidate: Int = fromBemidbarToVa_eschanan - weeksFromShavuotToAfterTishaBeAv

      if (combinefromBereishisToBemidbar < 0)
        (0, 0, combinefromBereishisToBemidbar + combineFromBemidbarToVa_eschananCandidate)
      else {
        // TODO clean this up, so there is no hardcoded assumption about combinableFromBereshitToVayikra.length == 1
        // and maybe rename it to ...beforTzav or something?
        val doCombineVayakhelPekudei: Boolean =
          (combinefromBereishisToBemidbar == combinableFromBereishisToVayikra.length + combinableFromVayikraToBemidbar.length) || {
            // This tweak is required only for the Holy Land (and never for AchareiMot) for some reason?
            val parshahBeforePesach: Parsha = Parsha.forIndex(weeksTo(shabbosBefore(Pesach(year))))
            !allowedBeforePesach.contains(parshahBeforePesach)
          }

        val combineFromBereishisToVayikra: Int = if (doCombineVayakhelPekudei) 1 else 0

        (
          combineFromBereishisToVayikra,
          combinefromBereishisToBemidbar - combineFromBereishisToVayikra,
          combineFromBemidbarToVa_eschananCandidate
        )
      }
    }

    val weeksFromVa_eschanan: Int = weeks.length - weeksToShavuot - weeksFromShavuotToAfterTishaBeAv
    val combineFromVa_eschanan: Int = fromVa_eschanan - weeksFromVa_eschanan

    val combine: Set[Parsha] =
      take(combinableFromBereishisToVayikra, combineFromBereishisToVayikra) ++
      take(combinableFromVayikraToBemidbar, combineFromVayikraToBemidbar) ++
      take(combinableFromBemidbarToVa_eschanan, combineFromBemidbarToVa_eschanan) ++
      take(combinableFromVa_eschanan, combineFromVa_eschanan)

    def process(toProcess: Seq[Parsha]): Seq[WeeklyReading] = toProcess match {
      case first :: second :: rest if combine.contains(first) =>
        WeeklyReading(first, Some(second)) +: process(rest)
      case first :: rest =>
        WeeklyReading(first, None) +: process(rest)
      case Nil =>  Nil
    }

    val result = process(Parsha.all.init)

    require(result.length == weeks.length)

    weeks zip result
  }

  private def take(what: Seq[Parsha], number: Int): Set[Parsha] = {
    require(0 <= number && number <= what.length)
    what.take(number).toSet
  }
}
