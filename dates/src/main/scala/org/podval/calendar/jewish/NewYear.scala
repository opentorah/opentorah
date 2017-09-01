package org.podval.calendar.jewish

import Jewish.{Day, Moment, TimeInterval}
import Day.Name._

// TODO KH 7:8 says that postponement of RoshHashonoh is done to align the calendar better with
// the true molad; analyze the statistics of distances between mean *and* true molad and RoshHashono.
// TODO are there meaningful names for the corrections?
object NewYear {

  sealed class Delay(val days: Int)
  object Delay {
    case object No extends Delay(0)
    case object Adu extends Delay(1)
    case object First extends Delay(1)
    case object FirstAndAdu extends Delay(2)
    case object Second extends Delay(2)
    case object Third extends Delay(1)
  }

  final def delay(yearNumber: Int, newMoon: Moment): Delay = {
    import Delay._
    if (isAdu(newMoon.day)) Adu /* KH 7:1 */
    else if (newMoon.time >= firstCorrection)
      if (!isAdu(newMoon.day.next)) First /* KH 7:2 */ else FirstAndAdu /* KH 7:3 */
    else if (
      (newMoon.day.name == Shlishi) &&
      (newMoon.time >= secondCorrection) &&
      !Cycle.isLeapYear(yearNumber)) Second  /* KH 7:4 */
    else if (
      (newMoon.day.name == Sheni) &&
      (newMoon.time >= thirdCorrection) &&
      // This is not defined for yer 0 - and doesn't apply :)
      Cycle.isLeapYear(yearNumber-1)) Third  /* KH 7:5 */
    else No
  }

  private final def isAdu(day: Day): Boolean = adu.contains(day.name)

  // KH 7:1
  private val adu: Set[Day.Name] = Set(Rishon, Rvii, Shishi)

  // KH 7:1-2 (molad zoken)
  final val firstCorrection: TimeInterval  = TimeInterval().hours(18)

  // KH 7:4
  final val secondCorrection: TimeInterval = TimeInterval().hours(9).parts(204)

  // KH 7:5
  // TODO this can be calculated based on the maximum length of a year and
  // the first correction; do it!
  final val thirdCorrection: TimeInterval  = TimeInterval().hours(15).parts(589)
}
