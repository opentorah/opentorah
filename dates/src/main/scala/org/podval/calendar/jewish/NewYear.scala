package org.podval.calendar.jewish

import Jewish.{Day, Moment, TimeVector}
import Day.Name._

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
      // This is not defined for year 0 - and doesn't apply :)
      Cycle.isLeapYear(yearNumber-1)) Third  /* KH 7:5 */
    else No
  }

  private final def isAdu(day: Day): Boolean = adu.contains(day.name)

  // KH 7:1
  private val adu: Set[Day.Name] = Set(Rishon, Rvii, Shishi)

  // KH 7:1-2 (molad zoken)
  final val firstCorrection: TimeVector  = TimeVector().hours(18)

  // KH 7:4
  final val secondCorrection: TimeVector = TimeVector().hours(9).parts(204)

  // KH 7:5
  final val thirdCorrection: TimeVector  = TimeVector().hours(15).parts(589)
}
