package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.{Jewish, NewYear}
import org.opentorah.calendar.jewish.SpecialDay.{RoshHashanah1, YomKippur}
import org.opentorah.texts.tanach.{Custom, Parsha}
import Jewish.{Day, Year}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class ShabbosShuvahTest extends AnyFlatSpec, Matchers:

  /** The Shabbos between Rosh Hashanah and Yom Kippur. */
  private def shabbosShuvahOf(year: Year): Day =
    var day = RoshHashanah1.date(year).next
    while !day.isShabbos do day = day.next
    day

  "Shabbos Shuvah" should "fall between Rosh Hashanah and Yom Kippur" in:
    for number <- NewYear.delaysEnabledFromYear to 6000 do
      val year = Year(number)
      val day = shabbosShuvahOf(year)
      day.isShabbos shouldBe true
      (day.number > RoshHashanah1.date(year).number) shouldBe true
      (day.number < YomKippur.date(year).number) shouldBe true

  it should "read Vayeilech's haftarah, whatever the parsha of the week" in:
    // Vayeilech is the parsha of that week only in the years when Nitzavim and
    // Vayeilech are read apart; otherwise Haazinu falls there, and used to keep
    // its own haftarah (Shiras David).
    var asVayeilech = 0
    var asHaazinu = 0
    for
      number <- NewYear.delaysEnabledFromYear to 6000
      inHolyLand <- Seq(false, true)
    do
      val year = Year(number)
      val day = shabbosShuvahOf(year)
      val reading = Schedule.get(day, inHolyLand).morning.get
      val weekly = Schedule.weeklyReadingsForYear(year, inHolyLand)(day)

      if weekly.parsha == Parsha.Vayeilech then asVayeilech += 1
      if weekly.parsha == Parsha.Haazinu then asHaazinu += 1

      for custom <- Seq(Custom.Ashkenaz, Custom.Sefard, Custom.Chabad,
                        Custom.Teiman, Custom.Italki, Custom.Frankfurt) do
        reading.haftarah.doFind(custom) shouldBe Some(Parsha.Vayeilech.haftarah.doFind(custom))

    // both shapes of year must actually be exercised
    asVayeilech should be > 0
    asHaazinu should be > 0

  "Haazinu after Yom Kippur" should "keep its own haftarah" in:
    // the years where Vayeilech falls on Shabbos Shuvah are exactly the years
    // where Haazinu falls after Yom Kippur, and there it is Shiras David.
    for
      number <- NewYear.delaysEnabledFromYear to 6000
      inHolyLand <- Seq(false, true)
    do
      val year = Year(number)
      val readings = Schedule.weeklyReadingsForYear(year, inHolyLand)
      val shuvah = shabbosShuvahOf(year)
      for
        (day, _) <- readings.find((_, wr) => wr.parsha == Parsha.Haazinu)
        if day.number != shuvah.number
      do
        val reading = Schedule.get(day, inHolyLand).morning.get
        reading.haftarah.doFind(Custom.Ashkenaz) shouldBe
          Some(Parsha.Haazinu.haftarah.doFind(Custom.Ashkenaz))
