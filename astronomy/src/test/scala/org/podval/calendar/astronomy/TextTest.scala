package org.podval.calendar.astronomy

import org.scalatest.FlatSpec
import org.podval.calendar.angle.AngleNumberSystem
import AngleNumberSystem.{Angle, AnglePoint, headRange, range}
import Zodiac.{Constellation, constellations}
import org.podval.calendar.jewish.{Cycle, Jewish}
import Jewish.{Day, Month, Year}
import org.podval.calendar.numbers.BigRational

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
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
    assertResult(12)(constellations.length)
    assertResult(AnglePoint(0))(Zodiac.Aries.start)
    constellations.init.zip(constellations.tail).foreach {
      case (prev: Constellation, next: Constellation) =>
        assertResult(prev.end)(prev.start + Angle(30))
        assertResult(next.start)(prev.end)
    }

    assertResult(Zodiac.Gemini  .at(Angle(10, 30, 40)))(AnglePoint(70, 30, 40))
    assertResult(Zodiac.Aquarius.at(Angle(20        )))(AnglePoint(320))
  }

  "angles" should "subtract as in KH 11:12" in {
    assertResult(AnglePoint(259, 29, 50))(AnglePoint(100, 20, 30) - Angle(200, 50, 40))
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
    assertResult(AnglePoint(105, 37, 25))(result.sunLongitudeMean)
    assertResult(Zodiac.Cancer.at(Angle(15, 37, 25)))(result.sunLongitudeMean)
  }

  "true Sun calculations" should "be as in KH 13:9-10" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Tammuz).day(14))
    assertResult(Day.Name.Shabbos)(result.day.name)
    assertResult(AnglePoint(105, 37, 25))(result.sunLongitudeMean)
    assertResult(AnglePoint(86, 45, 23))(result.sunApogee)
    assertResult(Angle(18, 52, 2))(result.sunCourseRaw)
    assertResult(Angle(19))(result.sunCourse)
    assertResult(-Angle(0, 38))(result.sunLongitudeCorrection)
    assertResult(AnglePoint(104, 59, 25))(result.sunLongitudeTrueRaw)
  }

  "true Moon calculations" should "be as in KH 15:8-9" in {
    val month: Month = Year(4938).month(Month.Name.Iyar)
    val day: Day = month.day(2)
    val newMoonDay: Day = month.newMoon.day
    // TODO what is the day of sighting? Not the day of the new moon... assertResult(day)(newMoonDay)
    val result = Calculator.Text.calculate(day)
    assertResult(Day.Name.Shishi)(result.day.name)
    assertResult(29)(result.daysAfterEpoch)
    assertResult(AnglePoint(35, 38, 33))(result.sunLongitudeMean)
    assertResult(AnglePoint(53, 36, 39))(result.moonLongitudeMeanAtTimeOfSighting)
    assertResult(AnglePoint(103, 21, 46))(result.moonAnomalyMean)
    assertResult(Angle(17, 58, 6))(result.elongation)
    assertResult(Angle(35, 56, 12))(result.doubleElongation)
    assertResult(Angle(5))(result.moonLongitudeDoubleElongationCorrection)
    // TODO printing error in standard editions: 180.
    // assertResult(AnglePoint(108, 21))(result.moonAnomalyTrue) // TODO got 108°21′46″
    assertResult(AnglePoint(108))(result.moonAnomalyTrue)
    // KH 15:9
    assertResult(-Angle(5, 1))(result.moonAnomalyVisible)
    // TODO printing error in standard editions: 33.
    assertResult(AnglePoint(48, 35, 39))(result.moonLongitudeTrueRaw)
    assertResult(Zodiac.Taurus.at(Angle(18, 36)))(result.moonLongitudeTrue)
  }

  "moon head calculations" should "be as in KH 16:4-5" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
    assertResult(Day.Name.Shishi)(result.day.name)
    assertResult(29)(result.daysAfterEpoch)
    // KH 16:5
    assertResult(AnglePoint(182, 29, 37))(result.moonHeadMeanReversed)
    assertResult(AnglePoint(177, 30, 23))(result.moonHeadMeanRaw)
    assertResult(Zodiac.Virgo.at(Angle(27, 30)))(result.moonHeadMean)
  }

  "interpolation of the lattitude" should "be as in KH 16:12" in {
    assertResult(Angle(3, 59))(Calculators.Text.moonLatitude(Angle(53)))
  }

  "quadranting of the lattitude" should "be as in KH 16:16-18" in {
    assertResult(Angle(2, 30))(Calculators.Text.moonLatitude(Angle(150)))
    assertResult(Angle(1, 43))(Calculators.Text.moonLatitude(Angle(200)))
    assertResult(Angle(4, 20))(Calculators.Text.moonLatitude(Angle(300)))
  }

  "moon lattitude calculations" should "be as in KH 16:19" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
    assertResult(Day.Name.Shishi)(result.day.name)
    assertResult(Zodiac.Taurus.at(Angle(18, 36)))(result.moonLongitudeTrue)
    assertResult(Zodiac.Virgo .at(Angle(27, 30)))(result.moonHeadMean)
    assertResult(Angle(231, 6))(result.moonLatitudeCourseRaw)
    assertResult(Angle(3, 53))(result.moonLatitude)
  }

  "arc of sighting calculations" should "be as in KH 17:13-14" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
    assertResult(Day.Name.Shishi)(result.day.name)
    assertResult(Zodiac.Taurus.at(Angle(7, 9)))(result.sunLongitudeTrue)
    assertResult(Zodiac.Taurus.at(Angle(18, 36)))(result.moonLongitudeTrue)
    assertResult(Angle( 3, 53))(result.moonLatitude)
    assertResult(result.moonLatitude)(result.latitude1)
    assertResult(false)(result.isMoonLatitudeNortherly)
    assertResult(Angle(11, 27))(result.longitude1)
    assertResult(Angle(1))(result.longitudeSightingAdjustment)
    assertResult(Angle(10, 27))(result.longitude2)
    assertResult(Angle( 0, 10))(result.latitudeSightingAdjustment)
    assertResult(Angle( 4, 3))(result.latitude2)
    assertResult(BigRational(1, 4))(result.moonCircuitPortion)
    assertResult(Angle( 1, 1))(result.moonCircuit)
    // KH 17:14
    assertResult(Angle(11, 28))(result.longitude3)
    // TODO "this longitude is in Taurus" - but longitude3 isn't, so I get 1/6 instead of 1/5...
    // Am I supposed to look at moonTrueLongitude?!
    assertResult(BigRational(1, 5))(result.moonLongitude3Portion)
    assertResult(Angle( 2, 18))(result.moonLongitude3Correction)
    assertResult(Angle(13, 46))(result.longitude4)
    assertResult(Angle( 2, 35))(result.geographicCorrection)
    assertResult(Angle(11, 11))(result.arcOfSighting)
  }

  "isSightable calculations" should "be as in KH 17:22" in {
    val result = Calculator.Text.calculate(Year(4938).month(Month.Name.Iyar).day(2))
    assertResult(Day.Name.Shishi)(result.day.name)
    assertResult(Angle(11, 11))(result.arcOfSighting)
    assertResult(Angle(11, 27))(result.longitude1)
    assertResult(true)(result.isMoonSightable)
  }
}
