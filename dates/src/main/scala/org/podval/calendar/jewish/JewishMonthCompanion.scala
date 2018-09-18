package org.podval.calendar.jewish

import org.podval.calendar.dates.MonthCompanion
import org.podval.judaica.metadata.NamesLoader

abstract class JewishMonthCompanion extends MonthCompanion[Jewish] {
  final override val Name: JewishMonthCompanion.type = JewishMonthCompanion

  final override def yearNumber(monthNumber: Int): Int = Cycle.monthYear(monthNumber)

  final override def numberInYear(monthNumber: Int): Int = Cycle.monthNumberInYear(monthNumber)
}

object JewishMonthCompanion extends NamesLoader {
  sealed trait Key extends KeyBase

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

  override def resourceName: String = "JewishMonth"
}
