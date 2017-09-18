package org.podval.calendar.astronomy

import org.scalatest.FlatSpec
import org.podval.calendar.angle.AngleNumberSystem
import AngleNumberSystem.{Angle, AnglePoint, headRange, range}
import Zodiac.{Constellation, constellations}
import org.podval.calendar.jewish.{Cycle, Jewish}
import Jewish.{Year, Month, Day}

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

    val (constellation1: Zodiac.Constellation, angle1: Angle) = Zodiac.fromAngle(AnglePoint(70, 30, 40))
    assertResult(Zodiac.Gemini)(constellation1)
    assertResult(Angle(10, 30, 40))(angle1)

    val (constellation2: Zodiac.Constellation, angle2: Angle) = Zodiac.fromAngle(AnglePoint(320))
    assertResult(Zodiac.Aquarius)(constellation2)
    assertResult(Angle(20))(angle2)
  }

  "angles" should "subtract as in KH 11:12" in {
    assertResult(AnglePoint(259, 29, 50))(AnglePoint(100, 20, 30) - Angle(200, 50, 40))
  }

  "epoch" should "be as in KH 11:16" in {
    assertResult(4938)(Cycle.yearInCycle(260, 17))
    assertResult(260)(Cycle.yearCycle(4938))
    assertResult(17)(Cycle.yearNumberInCycle(4938))
    assertResult(Year(4938).month(Month.Name.Nisan).day(3))(Epoch.epoch)
    assertResult(Day.Name.Chamishi)(Epoch.epoch.name)
  }

  "true Sun calculations" should "be as in KH 13:9-10" in {
    val nextDay: Day = Year(4938).month(Month.Name.Tammuz).day(14)

    assertResult(Day.Name.Shabbos)(nextDay.name)
    assertResult(100)(nextDay.number - Epoch.epoch.number)

    val nextLongitude: AnglePoint = Sun.longitudeMean(nextDay)
    assertResult(Sun.longitudeMeanAtEpoch + SunLongitudeMean.fromTable(100))(nextLongitude)
    assertResult(AnglePoint(105, 37, 25))(nextLongitude)
    val (constellation, angle) = Zodiac.fromAngle(nextLongitude)
    assertResult(Zodiac.Cancer)(constellation)
    assertResult(Angle(15, 37, 25))(angle)

    val nextApogee: AnglePoint = Sun.apogee(nextDay)
    assertResult(AnglePoint(86, 45, 23))(nextApogee)
    val course: Angle = nextLongitude - nextApogee
    assertResult(Angle(18, 52, 2))(course)

    assertResult(Angle(19))(course.roundTo(0))
    assertResult(Angle(0, -38))(SunLongitudeCorrection.fromTable(course))
    assertResult(AnglePoint(104, 59, 25))(Sun.longitudeTrue(nextDay))
  }

  "true Moon calculations" should "be as in KH 15:8-9" in {
    // TODO why all these roundTo()s? I am overcalculating it :)  Where does Rambam says to ignore the rest?
    val nextDay: Day = Year(4938).month(Month.Name.Iyar).day(2)

    assertResult(Day.Name.Shishi)(nextDay.name)
    assertResult(29)(nextDay.number - Epoch.epoch.number)
    val sunMean = Sun.longitudeMean(nextDay) //.roundToSeconds
    assertResult(AnglePoint(35, 38, 33))(sunMean)

    val moonMeanAtTimeOfSighting = Moon.longitudeMeanAtTimeOfSighting(nextDay, sunMean).roundTo(2)
    assertResult(AnglePoint(53, 36, 39))(moonMeanAtTimeOfSighting)

    val moonAnomalyMean = Moon.anomalyMean(nextDay).roundToSeconds
    assertResult(AnglePoint(103, 21, 46))(moonAnomalyMean)

    val elongation = moonMeanAtTimeOfSighting - sunMean
    assertResult(Angle(17, 58, 6))(elongation)
    val doubleElongation = elongation*2
    assertResult(Angle(35, 56, 12))(doubleElongation)
    val correction = MoonLongitudeDoubleElongationCorrection.correction(doubleElongation)
    assertResult(Angle(5))(correction)

    val moonAnomalyTrue = (moonAnomalyMean + correction).roundToMinutes
    // TODO printing error in standard editions: 180.
//    assertResult(AnglePoint(108, 21))(moonAnomalyTrue) // TODO 108°22′

    val anomalyVisible: Angle = MoonAnomalyVisible.fromTable(moonAnomalyTrue.toInterval).roundToMinutes
    assertResult(-Angle(5, 1))(anomalyVisible)

    val moonTrue = moonMeanAtTimeOfSighting + anomalyVisible
    // TODO printing error in standard editions: 33.
    assertResult(AnglePoint(48, 35, 39))(moonTrue)
    assertResult(AnglePoint(48, 35, 39))(Moon.longitudeTrueAtTimeOfSighting(nextDay))

    val (constellation, angle) = Zodiac.fromAngle(moonTrue.roundToMinutes)
    assertResult(Zodiac.Taurus)(constellation)
    assertResult(Angle(18, 36))(angle)
  }
}
