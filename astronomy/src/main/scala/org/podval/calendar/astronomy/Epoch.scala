package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Rotation, Position}
import org.podval.calendar.jewish.Jewish.{Day, Month, Year}

trait Epoch {
  def day: Day

  final def daysAfterEpoch(day: Day): Int = day.number - this.day.number

  def sunLongitudeMean: Position

  def sunApogee: Position

  def moonLongitudeMean: Position

  def moonAnomalyMean: Position

  def moonHeadMean: Position
}


object Epoch {
  object Text extends Epoch {
    final override val day: Day = Year(4938).month(Month.Name.Nisan).day(3)

    // KH 12:2
    final override def sunLongitudeMean: Position = Zodiac.Aries.at(Rotation(7, 3, 32))

    // KH 12:2
    final override def sunApogee: Position = Zodiac.Gemini.at(Rotation(26, 45, 8))

    // KH 14:4
    final override def moonLongitudeMean: Position = Zodiac.Taurus.at(Rotation(1, 14, 43))

    // KH 14:4
    final override def moonAnomalyMean: Position = Position(84, 28, 42)

    // KH 14:4; f7
    final override def moonHeadMean: Position = Position(180, 57, 28)
  }
}
