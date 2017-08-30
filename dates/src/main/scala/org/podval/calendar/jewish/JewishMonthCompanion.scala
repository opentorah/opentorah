package org.podval.calendar.jewish

import org.podval.calendar.dates.MonthCompanion
import org.podval.calendar.util.Named
import Jewish.{Moment, TimeInterval, Year}

abstract class JewishMonthCompanion extends MonthCompanion[Jewish] {
  final type Name = JewishMonthCompanion.Name

  final val Name: JewishMonthCompanion.Name.type = JewishMonthCompanion.Name

  // KH 6:3
  // TODO how is this really called? tropical?
  final val meanLunarPeriod: TimeInterval = TimeInterval().days(29).hours(12).parts(793)

  // Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2):
  // BeHaRaD: (KH 6:8)
  final val firstNewMoon: Moment = Moment().day(2).nightHours(5).parts(204)

  final override def yearNumber(monthNumber: Int): Int = {
    val cycleOfMonth = ((monthNumber - 1) / Year.monthsInCycle) + 1
    val yearsBeforeCycle = (cycleOfMonth - 1) * Year.yearsInCycle
    val yearMonthIsInCycle =
      Year.monthsBeforeYearInCycle.count(_ < numberInCycleOfMonth(monthNumber))
    yearsBeforeCycle + yearMonthIsInCycle
  }

  final override def numberInYear(monthNumber: Int): Int =
    numberInCycleOfMonth(monthNumber) - Year.firstMonthInCycle(yearNumber(monthNumber)) + 1

  private def numberInCycleOfMonth(monthNumber: Int): Int =
    ((monthNumber - 1) % Year.monthsInCycle) + 1
}


object JewishMonthCompanion {
  sealed class Name(val name: String) extends Named(name)

  object Name {
    case object Tishrei extends Name("Tishrei")
    case object Marheshvan extends Name("Marcheshvan")
    case object Kislev extends Name("Kislev")
    case object Teves extends Name("Teves")
    case object Shvat extends Name("Shevat")
    case object Adar extends Name("Adar")
    case object Nisan extends Name("Nissan")
    case object Iyar extends Name("Iyar")
    case object Sivan extends Name("Sivan")
    case object Tammuz extends Name("Tammuz")
    case object Av extends Name("Av")
    case object Elul extends Name("Elul")
    case object AdarI extends Name("Adar I")
    case object AdarII extends Name("Adar II")
  }
}
