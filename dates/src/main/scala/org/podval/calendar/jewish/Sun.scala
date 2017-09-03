package org.podval.calendar.jewish

import Jewish.TimeInterval
import org.podval.calendar.numbers.BigRational

// TODO add Zodiac class (in astronomy)
// TODO angular speed of the moon = 360 / (1/tropical month + 1/solar year) (in astronomy)
// TODO Which day of the week (+1/-1) was the Giving of the Law? (Sema)
// TODO Rambam's epoch - two days after molad?! (Petya Ofman)
object Sun {
  // KH 9:1
  val yearOfShmuel: TimeInterval = TimeInterval().days(365).hours(6)

  // KH 10:1
  val yearOfRavAda: TimeInterval = Cycle.cycleLength / Cycle.yearsInCycle

  val gregorianYear: TimeInterval = TimeInterval.fromRational(
    BigRational(365) +
    BigRational(1, 4) -
    BigRational(1, 100) +
    BigRational(1, 400))
}
