package org.podval.calendar.jewish

import org.podval.calendar.dates.MonthCompanion
import org.podval.calendar.util.Named
import Jewish.{Moment, TimeInterval}

abstract class JewishMonthCompanion extends MonthCompanion[Jewish] {
  final type Name = JewishMonthCompanion.Name

  final val Name: JewishMonthCompanion.Name.type = JewishMonthCompanion.Name

  final override def yearNumber(monthNumber: Int): Int = Cycle.monthYear(monthNumber)

  final override def numberInYear(monthNumber: Int): Int = Cycle.monthNumberInYear(monthNumber)
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

  final def newMoon(number: Int): Moment = firstNewMoon + meanLunarPeriod*(number-1)

  // KH 6:3
  // TODO how is this really called? tropical? Move out of here (to Moon?).
  final val meanLunarPeriod: TimeInterval = TimeInterval().days(29).hours(12).parts(793)

  // Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2):
  // BeHaRaD: (KH 6:8)
  final val firstNewMoon: Moment = Moment().day(2).nightHours(5).parts(204)
}
