package org.podval.calendar.jewish

import org.podval.calendar.dates.MonthCompanion
import org.podval.calendar.util.Named

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
}
