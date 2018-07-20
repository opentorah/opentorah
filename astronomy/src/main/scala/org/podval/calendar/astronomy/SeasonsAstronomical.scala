package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.{Position, Rotation}
import org.podval.calendar.jewish.{Jewish, Seasons}
import org.podval.calendar.jewish.Jewish.{Moment, Month, Year}
import org.podval.calendar.numbers.Math

final class SeasonsAstronomical(calculator: Calculator) extends Seasons {
  def tkufasNisan  (year: Year): Moment = tkufa(Zodiac.Aries    )(year)
  def tkufasTammuz (year: Year): Moment = tkufa(Zodiac.Cancer   )(year)
  def tkufasTishrei(year: Year): Moment = tkufa(Zodiac.Libra    )(year)
  def tkufasTeves  (year: Year): Moment = tkufa(Zodiac.Capricorn)(year)

  private def tkufa(zodiac: Zodiac)(year: Year): Moment = {
    def f(moment: Moment): Rotation = (sunLongitudeTrue(moment) - zodiac.start).symmetrical
    // TODO should this always be Nisan?
    val left: Moment = year.month(Month.Name.Nisan).prev.firstDay.toMoment
    val right: Moment = year.month(Month.Name.Nisan).next.firstDay.toMoment
    val result: Moment = Math.findZero[Jewish, Rotation](f, left, right, 0)
    result
  }

  private def sunLongitudeTrue(moment: Moment): Position = calculator.calculate(moment.day).sunLongitudeTrue
}
