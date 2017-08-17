package org.podval.calendar.gregorian

import org.podval.calendar.util.Named

sealed class GregorianMonthName(name: String) extends Named(name)

object GregorianMonthName {
  case object January extends GregorianMonthName("January")
  case object February extends GregorianMonthName("February")
  case object March extends GregorianMonthName("March")
  case object April extends GregorianMonthName("April")
  case object May extends GregorianMonthName("May")
  case object June extends GregorianMonthName("June")
  case object July extends GregorianMonthName("July")
  case object August extends GregorianMonthName("August")
  case object September extends GregorianMonthName("September")
  case object October extends GregorianMonthName("October")
  case object November extends GregorianMonthName("November")
  case object December extends GregorianMonthName("December")
}
