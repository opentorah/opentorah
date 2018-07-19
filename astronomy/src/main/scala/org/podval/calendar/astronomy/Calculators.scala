package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, Position}
import org.podval.calendar.numbers.BigRational

trait Calculators {
  def sunLongitudeMean: Int => Angle

  def sunApogee: Int => Angle

  def sunLongitudeCorrection: Angle => Angle

  def moonLongitudeMean: Int => Angle

  def moonLongitudeAdjustmentForTimeOfSighting: Position => Angle

  def moonAnomalyMean: Int => Angle

  def moonLongitudeDoubleElongationCorrection: Angle => Angle

  def moonAnomalyVisible: Angle => Angle

  def moonHeadMean: Int => Angle

  def moonLatitude: Angle => Angle

  def moonLongitudeSightingAdjustment: Position => Angle

  def moonLatitudeSightingAdjustment: Position => Angle

  def moonCircuitPortion: Position => BigRational

  def moonLongitude3Portion: Position => BigRational
}


object Calculators {
  object Text extends Calculators {
    final override def sunLongitudeMean: Int => Angle = SunLongitudeMean.calculate
    final override def sunApogee: Int => Angle = SunApogee.calculate
    final override def sunLongitudeCorrection: Angle => Angle = SunLongitudeCorrection.table.calculate
    final override def moonLongitudeMean: Int => Angle = MoonLongitudeMean.calculate
    final override def moonLongitudeAdjustmentForTimeOfSighting: Position => Angle =
      MoonLongitudeAdjustmentForTimeOfSighting.calculate
    final override def moonAnomalyMean: Int => Angle = MoonAnomalyMean.calculate
    final override def moonLongitudeDoubleElongationCorrection: Angle => Angle =
      MoonLongitudeDoubleElongationCorrection.calculate
    final override def moonAnomalyVisible: Angle => Angle = MoonAnomalyVisible.table.calculate
    final override def moonHeadMean: Int => Angle = MoonHeadMean.calculate
    final override def moonLatitude: Angle => Angle = MoonLatitude.table.calculate
    final override def moonLongitudeSightingAdjustment: Position => Angle =
      MoonLongitudeSightingAdjustment.calculate
    final override def moonLatitudeSightingAdjustment: Position => Angle =
      MoonLatitudeSightingAdjustment.calculate
    final override def moonCircuitPortion: Position => BigRational = MoonCircuitPortion.calculate
    final override def moonLongitude3Portion: Position => BigRational =
      MoonLongitude3Portion.calculate
  }

  // TODO gather all misprints into object Misprints extends Calculators
}
