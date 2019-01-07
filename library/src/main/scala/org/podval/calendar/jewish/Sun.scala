package org.podval.calendar.jewish

import Jewish.TimeVector
import org.podval.calendar.numbers.BigRational

object Sun {
  // KH 9:1
  val yearOfShmuel: TimeVector = TimeVector().days(365).hours(6)

  // KH 10:1
  val yearOfRavAda: TimeVector = LeapYearsCycle.cycleLength / LeapYearsCycle.yearsInCycle

//  val gregorianYear: TimeVector = TimeVector.fromRational(
//    BigRational(365) +
//    BigRational(1, 4) -
//    BigRational(1, 100) +
//    BigRational(1, 400))
}
