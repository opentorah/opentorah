package org.podval.calendar.astronomy

import org.podval.calendar.jewish.Jewish.Day
import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint, defaultLength}
import org.podval.calendar.numbers.BigRational

// TODO replace x, xRounded with xRaw, x

class Calculator(epoch: Epoch, calculators: Calculators, rounders: Rounders) {
  def calculate(day: Day): Calculator.Result = {
    val daysAfterEpoch: Int = epoch.daysAfterEpoch(day)

    // KH 12:2
    val sunLongitudeMean: AnglePoint =
      epoch.sunLongitudeMean + calculators.sunLongitudeMean(daysAfterEpoch)

    // KH 12:2
    // TODO Rambam says "the same way", but doesn't give value for 1 day...
    val sunApogee: AnglePoint = epoch.sunApogee + calculators.sunApogee(daysAfterEpoch)

    // KH 13:1-3,5-6 (maslul; mnas hamaslul)
    val sunCourse: Angle = sunLongitudeMean - sunApogee
    val sunCourseRounded: Angle = rounders.sunCourse(sunCourse)
    val sunLongitudeCorrection: Angle = calculators.sunLongitudeCorrection(sunCourseRounded)
    val sunLongitudeTrue: AnglePoint = sunLongitudeMean + sunLongitudeCorrection
    val sunLongitudeTrueRounded: AnglePoint = rounders.sunLongitudeTrue(sunLongitudeTrue)

    // TODO in KH 13:11, calculation of true solstices/equinoxes is mentioned,
    // but no algorithm is given.

    val moonLongitudeMean: AnglePoint =
      epoch.moonLongitudeMean + calculators.moonLongitudeMean(daysAfterEpoch)

    val moonLongitudeAdjustmentForTimeOfSighting: Angle =
      calculators.moonLongitudeAdjustmentForTimeOfSighting(sunLongitudeMean)

    val moonLongitudeMeanAtTimeOfSighting: AnglePoint =
      moonLongitudeMean + moonLongitudeAdjustmentForTimeOfSighting

    // KH 14:4
    val moonAnomalyMean: AnglePoint =
      epoch.moonAnomalyMean + calculators.moonAnomalyMean(daysAfterEpoch)

    // KH 15:1-3
    // TODO Moznaim Rambam, KH 15:1f2: double elongation = distance between moon's mean and apogee
    val elongation: Angle = moonLongitudeMeanAtTimeOfSighting - sunLongitudeMean
    val doubleElongation: Angle = elongation*2

    val moonLongitudeDoubleElongationCorrection: Angle =
      calculators.moonLongitudeDoubleElongationCorrection(doubleElongation)

    val moonAnomalyTrue: AnglePoint = moonAnomalyMean + moonLongitudeDoubleElongationCorrection
    val moonAnomalyTrueRounded: AnglePoint = rounders.moonAnomalyTrue(moonAnomalyTrue)

    // KH 15:4
    val moonAnomalyVisible: Angle =
      rounders.moonAnomalyVisible(calculators.moonAnomalyVisible(moonAnomalyTrueRounded.toInterval))

    val moonLongitudeTrue: AnglePoint = moonLongitudeMeanAtTimeOfSighting + moonAnomalyVisible
    val moonLongitudeTrueRounded: AnglePoint = rounders.moonLongitudeTrue(moonLongitudeTrue)

    // KH 16:3
    val moonHeadMeanReversed: AnglePoint = epoch.moonHeadMean + calculators.moonHeadMean(daysAfterEpoch)
    val moonHeadMean: AnglePoint = -moonHeadMeanReversed
    val moonHeadMeanRounded: AnglePoint = rounders.moonHeadMean(moonHeadMean)
    val moonTailMean: AnglePoint = moonHeadMean + Angle(180)
    val moonTailMeanRounded: AnglePoint = moonHeadMeanRounded + Angle(180)

    // KH 16:10
    val moonLatitudeCourse: Angle = (moonLongitudeTrueRounded - moonHeadMeanRounded).canonical
    val moonLatitudeCourseRounded: Angle = rounders.moonLatitudeCourse(moonLatitudeCourse)
    val isMoonLatitudeNortherly: Boolean = moonLatitudeCourseRounded < Angle(180)
    val moonLatitude: Angle = calculators.moonLatitude(moonLatitudeCourseRounded) // TODO AnglePoint

    // KH 17:1
    val longitude1: Angle = rounders.longitude1(moonLongitudeTrueRounded - sunLongitudeTrueRounded) // TODO AnglePoint
    // KH 17:2
    val latitude1: Angle = moonLatitude // TODO AnglePoint

    // KH 17:3-4
    val inNortherlyInclinedConstellations: Boolean = Zodiac.in(moonLongitudeTrueRounded, Set(
      Zodiac.Capricorn, Zodiac.Aquarius, Zodiac.Pisces, Zodiac.Aries, Zodiac.Taurus, Zodiac.Gemini
    ))

    val result1: Option[Boolean] =
      if (inNortherlyInclinedConstellations) {
        if (longitude1 <= Angle(9)) Some(false)
        else if (longitude1 > Angle(15)) Some(true)
        else None
      } else {
        if (longitude1 <= Angle(10)) Some(false)
        else if (longitude1 > Angle(24)) Some(true)
        else None
      }

    // KH 17:5-6
    val longitudeSightingAdjustment: Angle =
      calculators.moonLongitudeSightingAdjustment(moonLongitudeTrueRounded)
    val longitude2: Angle = rounders.longitude2(longitude1 - longitudeSightingAdjustment) // TODO AnglePoint

    // KH 17:7-9
    val latitudeSightingAdjustment: Angle =
      calculators.moonLatitudeSightingAdjustment(moonLongitudeTrueRounded)
    val latitude2: Angle =  // TODO AnglePoint
      if (isMoonLatitudeNortherly) latitude1 - latitudeSightingAdjustment
      else latitude1 + latitudeSightingAdjustment

    // KH 17:10
    val moonCircuitPortion: BigRational = calculators.moonCircuitPortion(moonLongitudeTrueRounded)
    val moonCircuit: Angle = rounders.moonCircuit(latitude2*(moonCircuitPortion, defaultLength))

    // KH 17:11
    val longitude3: Angle = // TODO AnglePoint
      rounders.longitude3(if (
        (isMoonLatitudeNortherly && inNortherlyInclinedConstellations) ||
        (!isMoonLatitudeNortherly && !inNortherlyInclinedConstellations)
      ) longitude2 - moonCircuit else longitude2 + moonCircuit)

    // KH 17:12
    val moonLongitude3Portion: BigRational =
      calculators.moonLongitude3Portion(moonLongitudeTrueRounded) /* TODO longitude3.toPoint?*/
    val moonLongitude3Correction: Angle =
      rounders.moonLongitude3Correction(longitude3*(moonLongitude3Portion, defaultLength))
    val longitude4: Angle = longitude3 + moonLongitude3Correction // TODO AnglePoint

    // KH 17:12
    val geographicCorrection: Angle =
      rounders.geographicCorrection(latitude1*(BigRational(2, 3), defaultLength))
    val arcOfSighting: Angle =
      rounders.arcOfSighting(if (isMoonLatitudeNortherly) longitude4 + geographicCorrection
      else longitude4 - geographicCorrection)

    val isMoonSightable: Boolean = result1.getOrElse {
      // KH 17:15
      if (arcOfSighting <= Angle(9)) false else
      if (arcOfSighting > Angle(14)) true else {
        // KH 17:16-21
        true // TODO can't understand what Rambam is saying: "from 9 to 10 or more than 10"?!
      }
    }

    Calculator.Result(
      day,
      daysAfterEpoch,
      sunLongitudeMean,
      sunApogee,
      sunCourse,
      sunCourseRounded,
      sunLongitudeCorrection,
      sunLongitudeTrue,
      sunLongitudeTrueRounded,
      moonLongitudeMean,
      moonLongitudeAdjustmentForTimeOfSighting,
      moonLongitudeMeanAtTimeOfSighting,
      moonAnomalyMean,
      elongation,
      doubleElongation,
      moonLongitudeDoubleElongationCorrection,
      moonAnomalyTrue,
      moonAnomalyTrueRounded,
      moonAnomalyVisible,
      moonLongitudeTrue,
      moonLongitudeTrueRounded,
      moonHeadMeanReversed,
      moonHeadMean,
      moonHeadMeanRounded,
      moonTailMean,
      moonTailMeanRounded,
      moonLatitudeCourse,
      moonLatitudeCourseRounded,
      moonLatitude,
      isMoonLatitudeNortherly,
      longitude1,
      latitude1,
      longitudeSightingAdjustment,
      latitudeSightingAdjustment,
      longitude2,
      latitude2,
      moonCircuitPortion,
      moonCircuit,
      longitude3,
      moonLongitude3Portion,
      moonLongitude3Correction,
      longitude4,
      geographicCorrection,
      arcOfSighting,
      isMoonSightable
    )
  }
}


object Calculator {

  case class Result(
    day: Day,
    daysAfterEpoch: Int,
    sunLongitudeMean: AnglePoint,
    sunApogee: AnglePoint,
    sunCourse: Angle,
    sunCourseRounded: Angle,
    sunLongitudeCorrection: Angle,
    sunLongitudeTrue: AnglePoint,
    sunLongitudeTrueRounded: AnglePoint,
    moonLongitudeMean: AnglePoint,
    moonLongitudeAdjustmentForTimeOfSighting: Angle,
    moonLongitudeMeanAtTimeOfSighting: AnglePoint,
    moonAnomalyMean: AnglePoint,
    elongation: Angle,
    doubleElongation: Angle,
    moonLongitudeDoubleElongationCorrection: Angle,
    moonAnomalyTrue: AnglePoint,
    moonAnomalyTrueRounded: AnglePoint,
    moonAnomalyVisible: Angle,
    moonLongitudeTrue: AnglePoint,
    moonLongitudeTrueRounded: AnglePoint,
    moonHeadMeanReversed: AnglePoint,
    moonHeadMean: AnglePoint,
    moonHeadMeanRounded: AnglePoint,
    moonTailMean: AnglePoint,
    moonTailMeanRounded: AnglePoint,
    moonLatitudeCourse: Angle,
    moonLatitudeCourseRounded: Angle,
    moonLatitude: Angle,
    isMoonLatitudeNortherly: Boolean,
    longitude1: Angle,
    latitude1: Angle,
    longitudeSightingAdjustment: Angle,
    latitudeSightingAdjustment: Angle,
    longitude2: Angle,
    latitude2: Angle,
    moonCircuitPortion: BigRational,
    moonCircuit: Angle,
    longitude3: Angle,
    moonLongitude3Portion: BigRational,
    moonLongitude3Correction: Angle,
    longitude4: Angle,
    geographicCorrection: Angle,
    arcOfSighting: Angle,
    isMoonSightable: Boolean
  )

  object Text extends Calculator(Epoch.Text, Calculators.Text, Rounders.Text)
}
