package org.podval.calendar.jewish

import Jewish.{Year, Moment, TimeInterval, interval}

// tkufos KH 9:3, 10:3
abstract class Seasons {
  // sun enters Tole (Aries)
  final def vernalEquinox(year: Year): Moment = springEquinox(year)
  final def springEquinox(year: Year): Moment = tkufasNisan(year)
  final def tkufasNisan(year: Year): Moment = tkufa(year, 0)

  // sun enters Sarton (Cancer)
  // TODO latin name
  final def summerSolstice(year: Year): Moment = tkufasTammuz(year)
  final def tkufasTammuz(year: Year): Moment = tkufa(year, 1)

  // sun enters Moznaim (Libra)
  // TODO latin name
  final def fallEquinox(year: Year): Moment = tkufasTishrei(year)
  final def tkufasTishrei(year: Year): Moment = tkufa(year, 2)

  // sun enters Gdi (Capricorn)
  // TODO latin name
  final def winterSolstice(year: Year): Moment = tkufasTeves(year)
  final def tkufasTeves(year: Year): Moment = tkufa(year, 3)

  final def tkufa(year: Year, number: Int): Moment =
    firstTkufasNisan + seasonLength * ((year.number - 1)*4 + number)

  final def seasonLength: TimeInterval = yearLength / 4

  def yearLength: TimeInterval

  final def firstTkufasNisan: Moment = Sun.firstMoladNisan - firstTkufasNisanBeforeFirstMoladNisan

  val firstTkufasNisanBeforeFirstMoladNisan: TimeInterval
}


object Seasons {
  object Shmuel extends Seasons {
    final override def yearLength: TimeInterval = Sun.yearOfShmuel

    // KH 9:3
    final override val firstTkufasNisanBeforeFirstMoladNisan: TimeInterval =
      interval.days(7).hours(9).parts(642)
  }

  object RavAda extends Seasons {
    final override def yearLength: TimeInterval = Sun.yearOfRavAda

    // TODO Rav Ada's tkufos started a *week* later than Shmuel's!
    // Isn't it readily observable?
    // Analyze the difference in historic periods...
    // KH 10:3
    final override val firstTkufasNisanBeforeFirstMoladNisan: TimeInterval =
      interval.hours(9).parts(642)
  }
}
