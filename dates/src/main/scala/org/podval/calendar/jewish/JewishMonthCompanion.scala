package org.podval.calendar.jewish

import org.podval.calendar.dates.MonthCompanion
import org.podval.calendar.metadata.{MetadataParser, Names, WithNames}

abstract class JewishMonthCompanion extends MonthCompanion[Jewish] {
  final val Name: JewishMonthCompanion.type = JewishMonthCompanion

  final type Name = Name.Name

  final override def yearNumber(monthNumber: Int): Int = Cycle.monthYear(monthNumber)

  final override def numberInYear(monthNumber: Int): Int = Cycle.monthNumberInYear(monthNumber)
}


object JewishMonthCompanion {
  sealed trait Name extends WithNames[Name] {
    def toNames: Map[Name, Names] = month2names
  }

  case object Tishrei extends Name
  case object Marheshvan extends Name
  case object Kislev extends Name
  case object Teves extends Name
  case object Shvat extends Name
  case object Adar extends Name
  case object Nisan extends Name
  case object Iyar extends Name
  case object Sivan extends Name
  case object Tammuz extends Name
  case object Av extends Name
  case object Elul extends Name
  case object AdarI extends Name { override def name: String = "Adar I"}
  case object AdarII extends Name { override def name: String = "Adar II"}

  val values: Seq[Name] =
    Seq(Tishrei, Marheshvan, Kislev, Teves, Shvat, Adar, Nisan, Iyar, Sivan, Tammuz, Av, Elul, AdarI, AdarII)

  private val month2names: Map[Name, Names] = MetadataParser.loadNames(this, "JewishMonth", values)
}
