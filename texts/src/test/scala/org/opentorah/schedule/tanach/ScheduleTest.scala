package org.opentorah.schedule.tanach

import org.opentorah.calendar.Week
import org.opentorah.calendar.jewish.{Jewish, NewYear, YearType}
import org.opentorah.calendar.jewish.SpecialDay.*
import org.opentorah.texts.tanach.{Parsha, WeeklyReading}
import org.opentorah.texts.tanach.Parsha.*
import Jewish.{Day, Year}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class ScheduleTest extends AnyFlatSpec, Matchers:

  "Torah readings" should "be assigned correctly" in {
    val start = System.currentTimeMillis()
    (NewYear.delaysEnabledFromYear to 6000).foreach(number =>
      val year = Year(number)

      verify(year, inHolyLand = false)
      verify(year, inHolyLand = true)
    )
    println("Time in ms: " + (System.currentTimeMillis()-start))
  }

  def verify(year: Year, inHolyLand: Boolean): Unit =
    val readings: Map[Day, WeeklyReading] = Schedule.weeklyReadingsForYear(year, inHolyLand)

    def findReadings(day: Day): WeeklyReading = readings(day)
    def isCombined(parsha: Parsha): Boolean = readings.exists(_._2.secondParsha.contains(parsha))

    // Pesach
    val readingsBeforePesach: WeeklyReading = findReadings(Pesach1.date(year).shabbosBefore)
    readingsBeforePesach.isCombined shouldBe false
    readingsBeforePesach.parsha shouldBe {
      if !year.isLeap then Tzav else if RoshHashanah1.date(year).is(Week.Day.Chamishi) then Acharei else Metzora
    }

    // Shavuot
    val readingsBeforeShavuot = findReadings(Shavuos1.date(year).shabbosBefore)
    readingsBeforeShavuot.isCombined shouldBe false
    Set[Parsha](Bemidbar, Nasso).contains(readingsBeforeShavuot.parsha) shouldBe true

    // Tisha Be Av
    findReadings(TishaBeAv.date(year).shabbosAfter) shouldBe WeeklyReading(Va_eschanan, None)

    // Rosh Ha Shanah
    val roshHaShanah: Day = RoshHashanah1.date(year+1)
    findReadings(roshHaShanah.shabbosBefore).parsha shouldBe Nitzavim
    isCombined(Vayeilech) shouldBe !roshHaShanah.is(Week.Day.Sheni) && !roshHaShanah.is(Week.Day.Shlishi)

    val combined: Set[Parsha] = readings.values.toSet.filter(_.isCombined).map(_.parsha)
    val yearType: YearType = YearType.forYear(year)
    val combinedFromStructure: Seq[Parsha] = ReadingStructure.all(yearType).combined(inHolyLand)
    combined shouldBe combinedFromStructure.toSet
