package org.podval.calendar.jewish

import org.podval.calendar.dates.MonthCompanion
import org.podval.calendar.metadata.{WithNames, WithNamesCompanion}

abstract class JewishMonthCompanion extends MonthCompanion[Jewish] {
  final val Name: JewishMonthCompanion.type = JewishMonthCompanion

  final type Name = JewishMonthName

  final override def yearNumber(monthNumber: Int): Int = Cycle.monthYear(monthNumber)

  final override def numberInYear(monthNumber: Int): Int = Cycle.monthNumberInYear(monthNumber)
}

sealed trait JewishMonthName extends WithNames[JewishMonthName] {
  def companion: WithNamesCompanion[JewishMonthName] = JewishMonthCompanion
}

object JewishMonthCompanion extends WithNamesCompanion[JewishMonthName] {
  case object Tishrei extends JewishMonthName
  case object Marheshvan extends JewishMonthName
  case object Kislev extends JewishMonthName
  case object Teves extends JewishMonthName
  case object Shvat extends JewishMonthName
  case object Adar extends JewishMonthName
  case object Nisan extends JewishMonthName
  case object Iyar extends JewishMonthName
  case object Sivan extends JewishMonthName
  case object Tammuz extends JewishMonthName
  case object Av extends JewishMonthName
  case object Elul extends JewishMonthName
  case object AdarI extends JewishMonthName { override def name: String = "Adar I"}
  case object AdarII extends JewishMonthName { override def name: String = "Adar II"}

  override val values: Seq[JewishMonthName] =
    Seq(Tishrei, Marheshvan, Kislev, Teves, Shvat, Adar, Nisan, Iyar, Sivan, Tammuz, Av, Elul, AdarI, AdarII)

  override def resourceName: String = "JewishMonth"
}
