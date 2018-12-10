package org.podval.calendar.jewish

import Jewish.{Moment, TimeVector, Month, Year}

abstract class SeasonsFixed  extends Seasons {
  final def tkufasNisan  (year: Year): Moment = tkufa(year, 0)
  final def tkufasTammuz (year: Year): Moment = tkufa(year, 1)
  final def tkufasTishrei(year: Year): Moment = tkufa(year, 2)
  final def tkufasTeves  (year: Year): Moment = tkufa(year, 3)

  final def tkufa(year: Year, number: Int): Moment =
    firstTkufasNisan + seasonLength * ((year.number - 1)*4 + number)

  def yearLength: TimeVector

  final def seasonLength: TimeVector = yearLength / 4

  final def firstTkufasNisan: Moment =
    SeasonsFixed.firstMoladNisan - firstTkufasNisanBeforeFirstMoladNisan

  val firstTkufasNisanBeforeFirstMoladNisan: TimeVector
}

object SeasonsFixed {
  // KH 6:8
  val firstMoladNisan: Moment = Year(1).month(Month.Name.Nisan).newMoon

  object Shmuel extends SeasonsFixed {
    final override def yearLength: TimeVector = Sun.yearOfShmuel

    // KH 9:3
    final override val firstTkufasNisanBeforeFirstMoladNisan: TimeVector =
      TimeVector().days(7).hours(9).parts(642)
  }

  object RavAda extends SeasonsFixed {
    final override def yearLength: TimeVector = Sun.yearOfRavAda

    // KH 10:3
    final override val firstTkufasNisanBeforeFirstMoladNisan: TimeVector =
      TimeVector().hours(9).parts(642)
  }
}
