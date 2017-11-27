package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem
import org.podval.calendar.jewish.{Jewish, Seasons}
import org.podval.calendar.jewish.Jewish.{Moment, Month, Year}
import org.podval.calendar.angle.AngleNumberSystem.AnglePoint
import org.podval.calendar.numbers.Math

final class SeasonsAstronomical(calculator: Calculator) extends Seasons {
  def tkufasNisan  (year: Year): Moment = tkufa(Zodiac.Aries    )(year)
  def tkufasTammuz (year: Year): Moment = tkufa(Zodiac.Cancer   )(year)
  def tkufasTishrei(year: Year): Moment = tkufa(Zodiac.Libra    )(year)
  def tkufasTeves  (year: Year): Moment = tkufa(Zodiac.Capricorn)(year)

  private def tkufa(constellation: Zodiac.Constellation)(year: Year): Moment = {
    def f(moment: Moment): AnglePoint =
      (sunLongitudeTrue(moment) - constellation.start).toPoint.symmetrical
    val left: Moment = year.month(Month.Name.Nisan).prev.firstDay.toMoment
    val right: Moment = year.month(Month.Name.Nisan).next.firstDay.toMoment
    val result: Moment = Math.findZero[Jewish, AngleNumberSystem](f, left, right, 0)
    result
  }

  private def sunLongitudeTrue(moment: Moment): AnglePoint = calculator.sunLongitudeTrue(moment.day)
}
