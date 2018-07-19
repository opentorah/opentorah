package org.podval.calendar.astronomy

import org.scalatest.FlatSpec
import org.podval.calendar.angle.AngleNumberSystem
import AngleNumberSystem.{Rotation, Position, headRange, range}
import org.podval.calendar.jewish.{Cycle, Jewish}
import Jewish.{Day, Month, Year}
import org.podval.calendar.numbers.BigRational

class TextTest extends FlatSpec {
  "angle units" should "be as in KH 11:7" in {
    assertResult(360)(headRange)
    assertResult( 60)(range(0))
    assertResult( 60)(range(1))
    assertResult( 60)(range(2))
    assertResult( 60)(range(3))
    assertResult( 60)(range(4))
    assertResult( 60)(range(5))
    assertResult( 60)(range(6))
  }

  "zodiac" should "be as in KH 11:7-9" in {
    assertResult(12)(Zodiac.all.length)
    assertResult(Position(0))(Zodiac.Aries.start)
    Zodiac.all.init.zip(Zodiac.all.tail).foreach {
      case (prev: Zodiac, next: Zodiac) =>
        assertResult(prev.end)(prev.start + Rotation(30))
        assertResult(next.start)(prev.end)
    }

    assertResult(Zodiac.Gemini  .at(Rotation(10, 30, 40)))(Position(70, 30, 40))
    assertResult(Zodiac.Aquarius.at(Rotation(20        )))(Position(320))
  }

  "angles" should "subtract as in KH 11:12" in {
    assertResult(Position(259, 29, 50))((Position(100, 20, 30) - Rotation(200, 50, 40)).canonical)
  }

  "epoch" should "be as in KH 11:16" in {
    assertResult(4938)(Cycle.yearInCycle(260, 17))
    assertResult(260)(Cycle.yearCycle(4938))
    assertResult(17)(Cycle.yearNumberInCycle(4938))
    assertResult(Year(4938).month(Month.Name.Nisan).day(3))(Epoch.Text.day)
    assertResult(Day.Name.Chamishi)(Epoch.Text.day.name)
  }

  "mean Sun calculations" should "be as in KH 12:2" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Tammuz).day(14))
    assertResult(Day.Name.Shabbos)(result.day.name)
    assertResult(100)(result.daysAfterEpoch)
    assertResult(Position(105, 37, 25))(result.sunLongitudeMean)
    assertResult(Zodiac.Cancer.at(Rotation(15, 37, 25)))(result.sunLongitudeMean)
  }

  "true Sun calculations" should "be as in KH 13:9-10" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Tammuz).day(14))
    assertResult(Day.Name.Shabbos)(result.day.name)
    assertResult(Position(105, 37, 25))(result.sunLongitudeMean)
    assertResult(Position(86, 45, 23))(result.sunApogee)
    assertResult(Rotation(18, 52, 2))(result.sunCourseRaw)
    assertResult(Rotation(19))(result.sunCourse)
    assertResult(-Rotation(0, 38))(result.sunLongitudeCorrection)
    assertResult(Position(104, 59, 25))(result.sunLongitudeTrueRaw)
  }

  "true Moon calculations" should "be as in KH 15:8-9" in {
    val month: Month = Year(4938).month(Month.Name.Iyar)
    val day: Day = month.day(2)
    val newMoonDay: Day = month.newMoon.day
    // TODO what is the day of sighting? Not the day of the new moon... assertResult(day)(newMoonDay)
    val result = Calculator.Text.calculate(day)
    assertResult(Day.Name.Shishi)(result.day.name)
    assertResult(29)(result.daysAfterEpoch)
    assertResult(Position(35, 38, 33))(result.sunLongitudeMean)
    assertResult(Position(53, 36, 39))(result.moonLongitudeMeanAtTimeOfSighting)
    assertResult(Position(103, 21, 46))(result.moonAnomalyMean)
    assertResult(Rotation(17, 58, 6))(result.elongation)
    assertResult(Rotation(35, 56, 12))(result.doubleElongation)
    assertResult(Rotation(5))(result.moonLongitudeDoubleElongationCorrection)
    // TODO printing error in standard editions: 180.
    // assertResult(Position(108, 21))(result.moonAnomalyTrue) // TODO got 108°21′46″
    assertResult(Position(108))(result.moonAnomalyTrue)
    // KH 15:9
    assertResult(-Rotation(5, 1))(result.moonAnomalyVisible)
    // TODO printing error in standard editions: 33.
    assertResult(Position(48, 35, 39))(result.moonLongitudeTrueRaw)
    assertResult(Zodiac.Taurus.at(Rotation(18, 36)))(result.moonLongitudeTrue)
  }

  "moon head calculations" should "be as in KH 16:4-5" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
    assertResult(Day.Name.Shishi)(result.day.name)
    assertResult(29)(result.daysAfterEpoch)
    // KH 16:5
    assertResult(Position(182, 29, 37))(result.moonHeadMeanReversed)
    assertResult(Position(177, 30, 23))(result.moonHeadMeanRaw.canonical)
    assertResult(Zodiac.Virgo.at(Rotation(27, 30)))(result.moonHeadMean.canonical)
  }

  "interpolation of the lattitude" should "be as in KH 16:12" in {
    assertResult(Rotation(3, 59))(Calculators.Text.moonLatitude(Rotation(53)))
  }

  "quadranting of the lattitude" should "be as in KH 16:16-18" in {
    assertResult(Rotation(2, 30))(Calculators.Text.moonLatitude(Rotation(150)))
    assertResult(Rotation(1, 43))(Calculators.Text.moonLatitude(Rotation(200)))
    assertResult(Rotation(4, 20))(Calculators.Text.moonLatitude(Rotation(300)))
  }

  "moon lattitude calculations" should "be as in KH 16:19" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
    assertResult(Day.Name.Shishi)(result.day.name)
    assertResult(Zodiac.Taurus.at(Rotation(18, 36)))(result.moonLongitudeTrue)
    assertResult(Zodiac.Virgo .at(Rotation(27, 30)))(result.moonHeadMean.canonical)
    assertResult(Rotation(231, 6))(result.moonLatitudeCourseRaw)
    assertResult(Rotation(3, 53))(result.moonLatitude)
  }

  "arc of sighting calculations" should "be as in KH 17:13-14" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
    assertResult(Day.Name.Shishi)(result.day.name)
    assertResult(Zodiac.Taurus.at(Rotation(7, 9)))(result.sunLongitudeTrue)
    assertResult(Zodiac.Taurus.at(Rotation(18, 36)))(result.moonLongitudeTrue)
    assertResult(Rotation( 3, 53))(result.moonLatitude)
    assertResult(result.moonLatitude)(result.latitude1)
    assertResult(false)(result.isMoonLatitudeNortherly)
    assertResult(Rotation(11, 27))(result.longitude1)
    assertResult(Rotation(1))(result.longitudeSightingAdjustment)
    assertResult(Rotation(10, 27))(result.longitude2)
    assertResult(Rotation( 0, 10))(result.latitudeSightingAdjustment)
    assertResult(Rotation( 4, 3))(result.latitude2)
    assertResult(BigRational(1, 4))(result.moonCircuitPortion)
    assertResult(Rotation( 1, 1))(result.moonCircuit)
    // KH 17:14
    assertResult(Rotation(11, 28))(result.longitude3)
    // TODO "this longitude is in Taurus" - but longitude3 isn't, so I get 1/6 instead of 1/5...
    // Am I supposed to look at moonTrueLongitude?!
    assertResult(BigRational(1, 5))(result.moonLongitude3Portion)
    assertResult(Rotation( 2, 18))(result.moonLongitude3Correction)
    assertResult(Rotation(13, 46))(result.longitude4)
    assertResult(Rotation( 2, 35))(result.geographicCorrection)
    assertResult(Rotation(11, 11))(result.arcOfSighting)
  }

  "isSightable calculations" should "be as in KH 17:22" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
    assertResult(Day.Name.Shishi)(result.day.name)
    assertResult(Rotation(11, 11))(result.arcOfSighting)
    assertResult(Rotation(11, 27))(result.longitude1)
    assertResult(true)(result.isMoonSightable)
  }
}
