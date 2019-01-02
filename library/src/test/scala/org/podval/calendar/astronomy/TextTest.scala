package org.podval.calendar.astronomy

import org.scalatest.{FlatSpec, Matchers}
import org.podval.calendar.angles.Angles
import Angles.{Position, Rotation, headRange, range}
import org.podval.calendar.jewish.{Cycle, Jewish}
import Jewish.{Day, Month, Year}
import org.podval.calendar.numbers.BigRational

class TextTest extends FlatSpec with Matchers {
  "angle units" should "be as in KH 11:7" in {
    headRange shouldBe 360
    range(0) shouldBe 60
    range(1) shouldBe 60
    range(2) shouldBe 60
    range(3) shouldBe 60
    range(4) shouldBe 60
    range(5) shouldBe 60
    range(6) shouldBe 60
  }

  "zodiac" should "be as in KH 11:7-9" in {
    Zodiac.all.length shouldBe 12
    Zodiac.Aries.start shouldBe Position(0)
    Zodiac.all.init.zip(Zodiac.all.tail).foreach {
      case (prev: Zodiac, next: Zodiac) =>
        (prev.start + Rotation(30)) shouldBe prev.end
        next.start shouldBe prev.end
    }

    Zodiac.Gemini  .at(Rotation(10, 30, 40)) shouldBe Position(70, 30, 40)
    Zodiac.Aquarius.at(Rotation(20        )) shouldBe Position(320)
  }

  "angles" should "subtract as in KH 11:12" in {
    (Position(100, 20, 30) - Rotation(200, 50, 40)).canonical shouldBe Position(259, 29, 50)
  }

  "epoch" should "be as in KH 11:16" in {
    Cycle.yearInCycle(260, 17) shouldBe 4938
    Cycle.yearCycle(4938) shouldBe 260
    Cycle.yearNumberInCycle(4938) shouldBe 17
    Epoch.Text.day shouldBe Year(4938).month(Month.Name.Nisan).day(3)
    Epoch.Text.day.name shouldBe Day.Name.Chamishi
  }

  "mean Sun calculations" should "be as in KH 12:2" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Tammuz).day(14))
    result.day.name shouldBe Day.Name.Shabbos
    result.daysAfterEpoch shouldBe 100
    result.sunLongitudeMean shouldBe Position(105, 37, 25)
    result.sunLongitudeMean shouldBe Zodiac.Cancer.at(Rotation(15, 37, 25))
  }

  "true Sun calculations" should "be as in KH 13:9-10" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Tammuz).day(14))
    result.day.name shouldBe Day.Name.Shabbos
    result.sunLongitudeMean shouldBe Position(105, 37, 25)
    result.sunApogee shouldBe Position(86, 45, 23)
    result.sunCourseRaw shouldBe Rotation(18, 52, 2)
    result.sunCourse shouldBe Rotation(19)
    result.sunLongitudeCorrection shouldBe -Rotation(0, 38)
    result.sunLongitudeTrueRaw shouldBe Position(104, 59, 25)
  }

  "true Moon calculations" should "be as in KH 15:8-9" in {
    val month: Month = Year(4938).month(Month.Name.Iyar)
    val day: Day = month.day(2)
    val newMoonDay: Day = month.newMoon.day
    // TODO what is the day of sighting? Not the day of the new moon... assertResult(day)(newMoonDay)
    val result = Calculator.Text.calculate(day)
    result.day.name shouldBe Day.Name.Shishi
    result.daysAfterEpoch shouldBe 29
    result.sunLongitudeMean shouldBe Position(35, 38, 33)
    result.moonLongitudeMeanAtTimeOfSighting shouldBe Position(53, 36, 39)
    result.moonAnomalyMean shouldBe Position(103, 21, 46)
    result.elongation shouldBe Rotation(17, 58, 6)
    result.doubleElongation shouldBe Rotation(35, 56, 12)
    result.moonLongitudeDoubleElongationCorrection shouldBe Rotation(5)
    // TODO printing error in standard editions: 180.
    // result.moonAnomalyTrue shouldBe Position(108, 21) // TODO got 108°21′46″
    result.moonAnomalyTrue shouldBe Position(108)
    // KH 15:9
    result.moonAnomalyVisible shouldBe -Rotation(5, 1)
    // TODO printing error in standard editions: 33.
    result.moonLongitudeTrueRaw shouldBe Position(48, 35, 39)
    result.moonLongitudeTrue shouldBe Zodiac.Taurus.at(Rotation(18, 36))
  }

  "moon head calculations" should "be as in KH 16:4-5" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
    result.day.name shouldBe Day.Name.Shishi
    result.daysAfterEpoch shouldBe 29
    // KH 16:5
    result.moonHeadMeanReversed shouldBe Position(182, 29, 37)
    result.moonHeadMeanRaw.canonical shouldBe Position(177, 30, 23)
    result.moonHeadMean.canonical shouldBe Zodiac.Virgo.at(Rotation(27, 30))
  }

  "interpolation of the lattitude" should "be as in KH 16:12" in {
    Calculators.Text.moonLatitude(Rotation(53)) shouldBe Rotation(3, 59)
  }

  "quadranting of the lattitude" should "be as in KH 16:16-18" in {
    Calculators.Text.moonLatitude(Rotation(150)) shouldBe Rotation(2, 30)
    Calculators.Text.moonLatitude(Rotation(200)) shouldBe Rotation(1, 43)
    Calculators.Text.moonLatitude(Rotation(300)) shouldBe Rotation(4, 20)
  }

  "moon lattitude calculations" should "be as in KH 16:19" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
    result.day.name shouldBe Day.Name.Shishi
    result.moonLongitudeTrue shouldBe Zodiac.Taurus.at(Rotation(18, 36))
    result.moonHeadMean.canonical shouldBe Zodiac.Virgo .at(Rotation(27, 30))
    result.moonLatitudeCourseRaw shouldBe Rotation(231, 6)
    result.moonLatitude shouldBe Rotation(3, 53)
  }

  "arc of sighting calculations" should "be as in KH 17:13-14" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
    result.day.name shouldBe Day.Name.Shishi
    result.sunLongitudeTrue shouldBe Zodiac.Taurus.at(Rotation(7, 9))
    result.moonLongitudeTrue shouldBe Zodiac.Taurus.at(Rotation(18, 36))
    result.moonLatitude shouldBe Rotation( 3, 53)
    result.latitude1 shouldBe result.moonLatitude
    result.isMoonLatitudeNortherly shouldBe false
    result.longitude1 shouldBe Rotation(11, 27)
    result.longitudeSightingAdjustment shouldBe Rotation(1)
    result.longitude2 shouldBe Rotation(10, 27)
    result.latitudeSightingAdjustment shouldBe Rotation( 0, 10)
    result.latitude2 shouldBe Rotation( 4, 3)
    result.moonCircuitPortion shouldBe BigRational(1, 4)
    result.moonCircuit shouldBe Rotation( 1, 1)
    // KH 17:14
    result.longitude3 shouldBe Rotation(11, 28)
    // TODO "this longitude is in Taurus" - but longitude3 isn't, so I get 1/6 instead of 1/5...
    // Am I supposed to look at moonTrueLongitude?!
    result.moonLongitude3Portion shouldBe BigRational(1, 5)
    result.moonLongitude3Correction shouldBe Rotation( 2, 18)
    result.longitude4 shouldBe Rotation(13, 46)
    result.geographicCorrection shouldBe Rotation( 2, 35)
    result.arcOfSighting shouldBe Rotation(11, 11)
  }

  "isSightable calculations" should "be as in KH 17:22" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
    result.day.name shouldBe Day.Name.Shishi
    result.arcOfSighting shouldBe Rotation(11, 11)
    result.longitude1 shouldBe Rotation(11, 27)
    result.isMoonSightable shouldBe true
  }
}
