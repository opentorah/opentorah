package org.podval.calendar.jewish

import org.podval.calendar.dates.MonthCompanion
import org.podval.judaica.metadata.{NamedCompanion, Named, Names}

abstract class JewishMonthCompanion extends MonthCompanion[Jewish] {
  final override val Name: JewishMonthCompanion.type = JewishMonthCompanion

  private[calendar] final override def yearNumber(monthNumber: Int): Int = LeapYearsCycle.monthYear(monthNumber)

  private[calendar] final override def numberInYear(monthNumber: Int): Int = LeapYearsCycle.monthNumberInYear(monthNumber)
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
