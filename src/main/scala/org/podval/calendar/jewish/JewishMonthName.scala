package org.podval.calendar.jewish

import org.podval.calendar.util.Named

sealed class JewishMonthName(val name: String) extends Named(name)

object JewishMonthName {
  case object Tishrei extends JewishMonthName("Tishrei")
  case object Marheshvan extends JewishMonthName("Marcheshvan")
  case object Kislev extends JewishMonthName("Kislev")
  case object Teves extends JewishMonthName("Teves")
  case object Shvat extends JewishMonthName("Shevat")
  case object Adar extends JewishMonthName("Adar")
  case object Nisan extends JewishMonthName("Nissan")
  case object Iyar extends JewishMonthName("Iyar")
  case object Sivan extends JewishMonthName("Sivan")
  case object Tammuz extends JewishMonthName("Tammuz")
  case object Av extends JewishMonthName("Av")
  case object Elul extends JewishMonthName("Elul")
  case object AdarI extends JewishMonthName("Adar I")
  case object AdarII extends JewishMonthName("Adar II")
}
