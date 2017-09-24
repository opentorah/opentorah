package org.podval.calendar.astronomy

import org.podval.calendar.jewish.Jewish.Day
import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}

// TODO virtualize calls to roundTo()

trait Calculator {
  def epoch: Day

  def sunLongitudeMeanAtEpoch: AnglePoint

  def sunLongitudeMeanCalculator: Int => Angle

  def sunApogeeAtEpoch: AnglePoint

  def sunApogeeCalculator: Int => Angle

  def sunLongitudeCorrectionCalculator: Angle => Angle

  def moonLongitudeMeanAtEpoch: AnglePoint

  def moonLongitudeMeanCalculator: Int => Angle

  def moonLongitudeAdjustmentForTimeOfSightingCalculator: AnglePoint => Angle

  def moonAnomalyMeanAtEpoch: AnglePoint

  def moonAnomalyMeanCalculator: Int => Angle

  def moonLongitudeDoubleElongationCorrectionCalculator: Angle => Angle

  def moonAnomalyVisibleCalculator: Angle => Angle

  def moonHeadMeanAtEpoch: AnglePoint

  def moonHeadMeanCalculator: Int => Angle

  def moonLattitudeCalculator: Angle => Angle

  def moonLongitudeSightingAdjustmentCalculator: AnglePoint => Angle
  def moonLattitudeSightingAdjustmentCalculator: AnglePoint => Angle

  final def calculate(day: Day): Calculator.Result = {
    val daysAfterEpoch: Int = day.number - epoch.number

    // KH 12:2
    val sunLongitudeMean: AnglePoint =
      sunLongitudeMeanAtEpoch + sunLongitudeMeanCalculator(daysAfterEpoch)

    // KH 12:2
    // TODO Rambam says "the same way", but doesn't give value for 1 day...
    val sunApogee: AnglePoint =
      sunApogeeAtEpoch + sunApogeeCalculator(daysAfterEpoch)

    // KH 13:1-3,5-6 (maslul; mnas hamaslul)
    val sunCourse: Angle = sunLongitudeMean - sunApogee
    val sunLongitudeCorrection: Angle = sunLongitudeCorrectionCalculator(sunCourse)
    val sunLongitudeTrue: AnglePoint = sunLongitudeMean + sunLongitudeCorrection

    // TODO in KH 13:11, calculation of true solstices/equinoxes is mentioned,
    // but no algorithm is given.

    val moonLongitudeMean: AnglePoint =
      moonLongitudeMeanAtEpoch + moonLongitudeMeanCalculator(daysAfterEpoch)

    val moonLongitudeAdjustmentForTimeOfSighting: Angle =
      moonLongitudeAdjustmentForTimeOfSightingCalculator(sunLongitudeMean)

    val moonLongitudeMeanAtTimeOfSighting: AnglePoint =
      moonLongitudeMean + moonLongitudeAdjustmentForTimeOfSighting

    // KH 14:4
    val moonAnomalyMean: AnglePoint =
      moonAnomalyMeanAtEpoch + moonAnomalyMeanCalculator(daysAfterEpoch)

    // KH 15:1-3
    // TODO Moznaim Rambam, KH 15:1f2: double elongation = distance between moon's mean and apogee
    val elongation: Angle = moonLongitudeMeanAtTimeOfSighting - sunLongitudeMean
    val doubleElongation: Angle = elongation*2

    val moonLongitudeDoubleElongationCorrection: Angle =
      moonLongitudeDoubleElongationCorrectionCalculator(doubleElongation)

    val moonAnomalyTrue: AnglePoint = moonAnomalyMean + moonLongitudeDoubleElongationCorrection

    // KH 15:4
    val moonAnomalyVisible: Angle =
      moonAnomalyVisibleCalculator(moonAnomalyTrue.toInterval).roundToMinutes

    val moonLongitudeTrue = moonLongitudeMeanAtTimeOfSighting + moonAnomalyVisible

    // KH 16:3
    val moonHeadMeanReversed: AnglePoint = moonHeadMeanAtEpoch + moonHeadMeanCalculator(daysAfterEpoch)
    val moonHeadMean: AnglePoint = -moonHeadMeanReversed
    val moonTailMean: AnglePoint = moonHeadMean + Angle(180)

    // KH 16:10
    val moonLattitudeCourse: Angle = moonLongitudeTrue.roundToMinutes - moonHeadMean.roundToMinutes
    val moonLattitude: Angle = moonLattitudeCalculator(moonLattitudeCourse)
    val isMoonLattitudeNortherly: Boolean = moonLattitude.isNegative // TODO

    // KH 17:1
    val longitude1: Angle = moonLongitudeTrue - sunLongitudeTrue
    // KH 17:2
    val lattitude1: Angle = moonLattitude

    // KH 17:3-4
    val result1: Option[Boolean] = {
      import Zodiac._
      if (in(moonLongitudeTrue, Set(Capricorn, Aquarius, Pisces, Aries, Taurus, Gemini))) {
        if (longitude1 <= Angle(9)) Some(false)
        else if (longitude1 > Angle(15)) Some(true)
        else None
      } else
      if (in(moonLongitudeTrue, Set(Cancer, Leo, Virgo, Libra, Scorpio, Sagittarius))) {
        if (longitude1 <= Angle(10)) Some(false)
        else if (longitude1 > Angle(24)) Some(true)
        else None
      } else
        throw new IllegalArgumentException
    }

    // KH 17:5-6
    val longitudeSightingAdjustment: Angle =
      moonLongitudeSightingAdjustmentCalculator(moonLongitudeTrue)
    val longitude2: Angle = longitude1 - longitudeSightingAdjustment

    // KH 17:7-9
    val lattitudeSightingAdjustment: Angle =
      moonLattitudeSightingAdjustmentCalculator(moonLongitudeTrue)
    val lattitude2: Angle =
      if (isMoonLattitudeNortherly) lattitude1 - lattitudeSightingAdjustment
      else lattitude1 + lattitudeSightingAdjustment

    val result: Boolean = result1.getOrElse {
      false // TODO
    }

    Calculator.Result(
      day,
      daysAfterEpoch,
      sunLongitudeMean,
      sunApogee,
      sunCourse,
      sunLongitudeCorrection,
      sunLongitudeTrue,
      moonLongitudeMean,
      moonLongitudeAdjustmentForTimeOfSighting,
      moonLongitudeMeanAtTimeOfSighting,
      moonAnomalyMean,
      elongation,
      doubleElongation,
      moonLongitudeDoubleElongationCorrection,
      moonAnomalyTrue,
      moonAnomalyVisible,
      moonLongitudeTrue,
      moonHeadMeanReversed,
      moonHeadMean,
      moonTailMean,
      moonLattitudeCourse,
      moonLattitude,
      longitude1,
      lattitude1,
      longitudeSightingAdjustment,
      lattitudeSightingAdjustment,
      longitude2,
      lattitude2
    )
  }
}


object Calculator {
  object TableCalculator extends Calculator {
    final override def epoch: Day = Epoch.epoch
    final override def sunLongitudeMeanAtEpoch: AnglePoint = SunLongitudeMean.atEpoch
    final override def sunLongitudeMeanCalculator: Int => Angle = SunLongitudeMean.table.calculate
    final override def sunApogeeAtEpoch: AnglePoint = SunApogee.atEpoch
    final override def sunApogeeCalculator: Int => Angle = SunApogee.table.calculate
    final override def sunLongitudeCorrectionCalculator: Angle => Angle =
      SunLongitudeCorrection.table.calculate
    final override def moonLongitudeMeanAtEpoch: AnglePoint = MoonLongitudeMean.atEpoch
    final override def moonLongitudeMeanCalculator: Int => Angle =
      MoonLongitudeMean.table.calculate
    final override def moonLongitudeAdjustmentForTimeOfSightingCalculator: AnglePoint => Angle =
      MoonLongitudeAdjustmentForTimeOfSighting.calculate
    final override def moonAnomalyMeanAtEpoch: AnglePoint = MoonAnomalyMean.atEpoch
    final override def moonAnomalyMeanCalculator: Int => Angle = MoonAnomalyMean.table.calculate
    final def moonLongitudeDoubleElongationCorrectionCalculator: Angle => Angle =
      MoonLongitudeDoubleElongationCorrection.calculate
    final override def moonAnomalyVisibleCalculator: Angle => Angle =
      MoonAnomalyVisible.table.calculate
    final override def moonHeadMeanAtEpoch: AnglePoint = MoonHeadMean.atEpoch
    final override def moonHeadMeanCalculator: Int => Angle = MoonHeadMean.table.calculate
    final override def moonLattitudeCalculator: Angle => Angle = MoonLattitude.table.calculate
    final override def moonLongitudeSightingAdjustmentCalculator: AnglePoint => Angle =
      MoonLongitudeSightingAdjustment.calculate
    final override def moonLattitudeSightingAdjustmentCalculator: AnglePoint => Angle =
      MoonLattitudeSightingAdjustment.calculate
  }

  case class Result(
    day: Day,
    daysAfterEpoch: Int,
    sunLongitudeMean: AnglePoint,
    sunApogee: AnglePoint,
    sunCourse: Angle,
    sunLongitudeCorrection: Angle,
    sunLongitudeTrue: AnglePoint,
    moonLongitudeMean: AnglePoint,
    moonLongitudeAdjustmentForTimeOfSighting: Angle,
    moonLongitudeMeanAtTimeOfSighting: AnglePoint,
    moonAnomalyMean: AnglePoint,
    elongation: Angle,
    doubleElongation: Angle,
    moonLongitudeDoubleElongationCorrection: Angle,
    moonAnomalyTrue: AnglePoint,
    moonAnomalyVisible: Angle,
    moonLongitudeTrue: AnglePoint,
    moonHeadMeanReversed: AnglePoint,
    moonHeadMean: AnglePoint,
    moonTailMean: AnglePoint,
    moonLattitudeCourse: Angle,
    moonLattitude: Angle,
    longitude1: Angle,
    lattitude1: Angle,
    longitudeSightingAdjustment: Angle,
    lattitudeSightingAdjustment: Angle,
    longitude2: Angle,
    lattitude2: Angle
  )
}
