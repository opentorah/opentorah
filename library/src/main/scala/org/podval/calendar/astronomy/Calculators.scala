package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.{Position, Rotation}
import org.podval.calendar.numbers.BigRational

trait Calculators {
  def sunLongitudeMean: Int => Rotation

  def sunApogee: Int => Rotation

  def sunLongitudeCorrection: Rotation => Rotation

  def moonLongitudeMean: Int => Rotation

  def moonLongitudeAdjustmentForTimeOfSighting: Position => Rotation

  def moonAnomalyMean: Int => Rotation

  def moonLongitudeDoubleElongationCorrection: Rotation => Rotation

  def moonAnomalyVisible: Position => Rotation

  def moonHeadMean: Int => Rotation

  def moonLatitude: Rotation => Rotation

  def moonLongitudeSightingAdjustment: Position => Rotation

  def moonLatitudeSightingAdjustment: Position => Rotation

  def moonCircuitPortion: Position => BigRational

  def moonLongitude3Portion: Position => BigRational
}


object Calculators {
  object Text extends Calculators {
    final override def sunLongitudeMean: Int => Rotation = SunLongitudeMean.calculate
    final override def sunApogee: Int => Rotation = SunApogee.calculate
    final override def sunLongitudeCorrection: Rotation => Rotation = SunLongitudeCorrection.table.calculate
    final override def moonLongitudeMean: Int => Rotation = MoonLongitudeMean.calculate
    final override def moonLongitudeAdjustmentForTimeOfSighting: Position => Rotation =
      MoonLongitudeAdjustmentForTimeOfSighting.calculate
    final override def moonAnomalyMean: Int => Rotation = MoonAnomalyMean.calculate
    final override def moonLongitudeDoubleElongationCorrection: Rotation => Rotation =
      MoonLongitudeDoubleElongationCorrection.calculate
    final override def moonAnomalyVisible: Position => Rotation = MoonAnomalyVisible.table.calculate
    final override def moonHeadMean: Int => Rotation = MoonHeadMean.calculate
    final override def moonLatitude: Rotation => Rotation = MoonLatitude.table.calculate
    final override def moonLongitudeSightingAdjustment: Position => Rotation =
      MoonLongitudeSightingAdjustment.calculate
    final override def moonLatitudeSightingAdjustment: Position => Rotation =
      MoonLatitudeSightingAdjustment.calculate
    final override def moonCircuitPortion: Position => BigRational = MoonCircuitPortion.calculate
    final override def moonLongitude3Portion: Position => BigRational =
      MoonLongitude3Portion.calculate
  }

  // TODO gather all misprints into object Misprints extends Calculators
}
