package org.podval.calendar.jewish

import Jewish.{Year, Moment}

// tkufos KH 9:3, 10:3
abstract class Seasons {
  // sun enters Tele (Aries)
  final def vernalEquinox(year: Year): Moment = springEquinox(year)
  final def springEquinox(year: Year): Moment = tkufasNisan(year)
  def tkufasNisan(year: Year): Moment

  // sun enters Sarton (Cancer)
  // TODO latin name
  final def summerSolstice(year: Year): Moment = tkufasTammuz(year)
  def tkufasTammuz(year: Year): Moment

  // sun enters Moznaim (Libra)
  // TODO latin name
  final def fallEquinox(year: Year): Moment = tkufasTishrei(year)
  def tkufasTishrei(year: Year): Moment

  // sun enters Gdi (Capricorn)
  // TODO latin name
  final def winterSolstice(year: Year): Moment = tkufasTeves(year)
  def tkufasTeves(year: Year): Moment
}
