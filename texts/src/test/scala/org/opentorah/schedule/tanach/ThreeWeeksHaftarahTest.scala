package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.Jewish
import org.opentorah.calendar.jewish.SpecialDay.FastOfTammuz
import org.opentorah.texts.tanach.{Custom, Haftarah, Parsha, WeeklyReading}
import Jewish.{Day, Year}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The three haftarot of rebuke belong to the three Shabbosim of the Three
 * Weeks -- between the Fast of Tammuz and Tisha BeAv. Which parshiyos those
 * are depends on whether Mattos and Masei are combined, so the haftarot shift
 * with them: when they are combined there is one Shabbos fewer, Pinchas falls
 * inside the Three Weeks and reads Mattos's haftarah, and the combined week
 * reads Masei's.
 */
final class ThreeWeeksHaftarahTest extends AnyFlatSpec, Matchers:

  private val customs: Seq[Custom] = Seq(Custom.Ashkenaz, Custom.Sefard,
    Custom.Chabad, Custom.Teiman, Custom.Italki, Custom.Frankfurt)

  /** Two centuries: both shapes of year arise many times over. */
  private val years: Range = 5700 to 5900

  private def readings(year: Year, inHolyLand: Boolean): Map[Day, WeeklyReading] =
    Schedule.weeklyReadingsForYear(year, inHolyLand)

  private def dayOf(readings: Map[Day, WeeklyReading], parsha: Parsha): Option[Day] =
    readings.find((_, weekly) => weekly.parsha == parsha).map(_._1)

  /**
   * The parsha's haftarah, possibly with more after it: Masei usually falls on
   * Shabbos Rosh Chodesh Av, which adds the Rosh Chodesh haftarah. What matters
   * here is which parsha's haftarah the week begins with.
   */
  private def beginsWith(actual: Option[Haftarah], base: Haftarah): Unit =
    withClue(s"expected to begin with $base but was $actual: ")(
      actual.exists(_.spans.startsWith(base.spans)) shouldBe true)

  /**
   * Schedule.get for the single day, not Schedule(year): only a handful of days
   * a year are looked at here, and building the whole year to reach them costs
   * more than building each of them on its own.
   */
  private def haftarahOn(day: Day, inHolyLand: Boolean, custom: Custom): Option[Haftarah] =
    Schedule.get(day, inHolyLand).morning.get.haftarah.doFind(custom)

  "the weeks of rebuke" should "shift when Mattos and Masei are combined" in:
    var combinedYears = 0
    var separateYears = 0
    for
      number <- years
      inHolyLand <- Seq(false, true)
    do
      val year = Year(number)
      val weekly = readings(year, inHolyLand)
      val combined = weekly.exists(_._2.secondParsha.contains(Parsha.Masei))

      val pinchas = dayOf(weekly, Parsha.Pinchas).get
      withClue(s"$number holyLand=$inHolyLand: ")(
        (pinchas.number > FastOfTammuz.date(year).number) shouldBe combined)

      val maseiWeek = weekly.find((_, w) =>
        (w.parsha == Parsha.Masei) || w.secondParsha.contains(Parsha.Masei)).get._1

      for custom <- customs do
        withClue(s"$number holyLand=$inHolyLand combined=$combined $custom: ") {
          // the Masei week reads Masei's haftarah, combined or not
          beginsWith(haftarahOn(maseiWeek, inHolyLand, custom),
                     Parsha.Masei.haftarah.doFind(custom))

          if combined then
            // Mattos's haftarah moves onto Pinchas
            beginsWith(haftarahOn(pinchas, inHolyLand, custom),
                       Parsha.Mattos.haftarah.doFind(custom))
          else
            // Pinchas keeps its own, and Mattos is read on its own week
            beginsWith(haftarahOn(pinchas, inHolyLand, custom),
                       Parsha.Pinchas.haftarah.doFind(custom))
            beginsWith(haftarahOn(dayOf(weekly, Parsha.Mattos).get, inHolyLand, custom),
                       Parsha.Mattos.haftarah.doFind(custom))
        }

      if combined then combinedYears += 1 else separateYears += 1

    // both shapes of year must actually arise, or this proves nothing
    combinedYears should be > 0
    separateYears should be > 0

  it should "read all three rebukes, in order, every year" in:
    for
      number <- years
      inHolyLand <- Seq(false, true)
    do
      val year = Year(number)
      val weeks: Seq[Day] = readings(year, inHolyLand).keys.toSeq
        .filter(_.number > FastOfTammuz.date(year).number)
        .sortBy(_.number)
        .take(3)
      val wanted = Seq(Parsha.Mattos, Parsha.Masei, Parsha.Devarim)
        .map(_.haftarah.doFind(Custom.Ashkenaz))
      withClue(s"$number holyLand=$inHolyLand: ")(
        weeks.zip(wanted).foreach((day, base) =>
          beginsWith(haftarahOn(day, inHolyLand, Custom.Ashkenaz), base)))
