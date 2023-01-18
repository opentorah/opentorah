package org.opentorah.astronomy

import org.opentorah.calendar.jewish.{Jewish, Season}
import Jewish.{Moment, Month, Year}
import org.opentorah.numbers.Math

// In KH 13:11, calculation of true solstices/equinoxes is mentioned, but no algorithm is given.
final class SeasonsAstronomical(calculator: Calculator) extends Season.ForYear:
  override def seasonForYear(season: Season, year: Year): Moment =
    def f(moment: Moment): Angles.Rotation = sunLongitudeTrue(moment) - season.sunEnters.start
    val left: Moment = year.month(Month.Nisan).prev.firstDay.toMoment
    val right: Moment = year.month(Month.Nisan).next.firstDay.toMoment
    val result: Moment = Math.findZero(Jewish, Angles)(f, left, right, 0)
    result

  private def sunLongitudeTrue(moment: Moment): Angles.Position = calculator.calculate(moment.day).sunLongitudeTrue
