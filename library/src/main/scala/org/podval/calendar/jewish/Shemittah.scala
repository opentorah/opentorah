package org.podval.calendar.jewish

object Shemittah {
  val yearsInCycle: Int = 7

  def yearNumberInCycle(year: Jewish.Year): Int = year.number % yearsInCycle + 1
}
