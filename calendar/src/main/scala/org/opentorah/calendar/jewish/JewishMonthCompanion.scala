package org.opentorah.calendar.jewish

import org.opentorah.dates.MonthCompanion
import org.opentorah.metadata.{Named, NamedCompanion, Names}

abstract class JewishMonthCompanion(calendar: Jewish) extends MonthCompanion[Jewish](calendar) {
  final override val Name: JewishMonthCompanion.type = JewishMonthCompanion

  private[opentorah] final override def yearNumber(monthNumber: Int): Int = LeapYearsCycle.monthYear(monthNumber)

  private[opentorah] final override def numberInYear(monthNumber: Int): Int = LeapYearsCycle.monthNumberInYear(monthNumber)
}

object JewishMonthCompanion extends NamedCompanion {
  sealed trait Key extends Named {
    final override def names: Names = toNames(this)
  }

  case object Tishrei extends Key
  case object Marheshvan extends Key
  case object Kislev extends Key
  case object Teves extends Key
  case object Shvat extends Key
  case object Adar extends Key
  case object Nisan extends Key
  case object Iyar extends Key
  case object Sivan extends Key
  case object Tammuz extends Key
  case object Av extends Key
  case object Elul extends Key
  case object AdarI extends Key { override def name: String = "Adar I"}
  case object AdarII extends Key { override def name: String = "Adar II"}

  override val values: Seq[Key] =
    Seq(Tishrei, Marheshvan, Kislev, Teves, Shvat, Adar, Nisan, Iyar, Sivan, Tammuz, Av, Elul, AdarI, AdarII)

  protected override def resourceName: String = "JewishMonth"
}
