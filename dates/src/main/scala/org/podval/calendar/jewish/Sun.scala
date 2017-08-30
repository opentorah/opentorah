package org.podval.calendar.jewish

import Jewish.{interval, Year, Month, TimeInterval, Moment}

// TODO add Zodiac class (in astronomy)
// TODO angular speed of the moon = 360 / (1/tropical month + 1/solar year) (in astronomy)
// TODO Which day of the week (+1/-1) was the Giving of the Law? (Sema)
// TODO Rambam's epoch - two days after molad?! (Petya Ofman)
object Sun {
  // KH 6:8
  val firstMoladNisan: Moment = Year(1).month(Month.Name.Nisan).newMoon

  // KH 9:1
  val yearOfShmuel: TimeInterval = interval.days(365).hours(6)

  // KH 10:1
  val yearOfRavAda: TimeInterval = Year.cycleLength / Year.yearsInCycle
}
