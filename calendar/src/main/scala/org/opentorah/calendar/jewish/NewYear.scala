package org.opentorah.calendar.jewish

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
    val dayName: Day.Name = name(newMoon.dayNumber)
    if (adu.contains(dayName)) Adu /* KH 7:1 */
    else if (newMoon.time >= firstCorrection)
      if (!adu.contains(name(newMoon.dayNumber+1))) First /* KH 7:2 */ else FirstAndAdu /* KH 7:3 */
    else if (
      (dayName == Shlishi) &&
      (newMoon.time >= secondCorrection) &&
      !LeapYearsCycle.isLeapYear(yearNumber)) Second  /* KH 7:4 */
    else if (
      (dayName == Sheni) &&
      (newMoon.time >= thirdCorrection) &&
      // This is not defined for year 0 - and doesn't apply :)
      LeapYearsCycle.isLeapYear(yearNumber-1)) Third  /* KH 7:5 */
    else No
  }

  // KH 7:1
  private val adu: Set[Day.Name] = Set(Rishon, Rvii, Shishi)

  // KH 7:1-2 (molad zoken)
  final val firstCorrection: TimeVector  = TimeVector().hours(18)

  // KH 7:4
  final val secondCorrection: TimeVector = TimeVector().hours(9).parts(204)

  // KH 7:5
  final val thirdCorrection: TimeVector  = TimeVector().hours(15).parts(589)

  final def name(dayNumber: Int): Day.Name = Day.names(Day.numberInWeek(dayNumber) - 1)
}
