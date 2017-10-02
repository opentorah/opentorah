package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.AnglePoint
import org.podval.calendar.jewish.Jewish.{Year, Month, Day}

trait Epoch {
  def day: Day

  final def daysAfterEpoch(day: Day): Int = day.number - this.day.number

  def sunLongitudeMean: AnglePoint

  def sunApogee: AnglePoint

  def moonLongitudeMean: AnglePoint

  def moonAnomalyMean: AnglePoint

  def moonHeadMean: AnglePoint
}


object Epoch {
  object Text extends Epoch {
    final override val day: Day = Year(4938).month(Month.Name.Nisan).day(3)

    final override def sunLongitudeMean: AnglePoint = SunLongitudeMean.atEpoch
    final override def sunApogee: AnglePoint = SunApogee.atEpoch
    final override def moonLongitudeMean: AnglePoint = MoonLongitudeMean.atEpoch
    final override def moonAnomalyMean: AnglePoint = MoonAnomalyMean.atEpoch
    final override def moonHeadMean: AnglePoint = MoonHeadMean.atEpoch
  }
}
