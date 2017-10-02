package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}
import org.podval.calendar.jewish.Jewish.{Day, Month, Year}

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

    // KH 12:2
    final override def sunLongitudeMean: AnglePoint = Zodiac.Aries.at(Angle(7, 3, 32))

    // KH 12:2
    final override def sunApogee: AnglePoint = Zodiac.Gemini.at(Angle(26, 45, 8))

    // KH 14:4
    final override def moonLongitudeMean: AnglePoint = Zodiac.Taurus.at(Angle(1, 14, 43))

    // KH 14:4
    final override def moonAnomalyMean: AnglePoint = AnglePoint(84, 28, 42)

    // KH 14:4; f7
    final override def moonHeadMean: AnglePoint = AnglePoint(180, 57, 28)
  }
}
