package org.podval.calendar.jewish

import Jewish.TimeInterval
import org.podval.calendar.numbers.BigRational

object Sun {
  // KH 9:1
  val yearOfShmuel: TimeInterval = TimeInterval().days(365).hours(6)

  // KH 10:1
  val yearOfRavAda: TimeInterval = Cycle.cycleLength / Cycle.yearsInCycle

  // TODO move or remove
//  val gregorianYear: TimeInterval = TimeInterval.fromRational(
//    BigRational(365) +
//    BigRational(1, 4) -
//    BigRational(1, 100) +
//    BigRational(1, 400))
}
