package org.podval.calendar.dates.jewish

import Jewish.{Day, Month, Year}

object NewMoon {
  def print(yearNumber: Int): Unit = {
    val year: Year = Year(yearNumber)
    for { month <- year.months } yield {

    }
  }

  def main(args: Array[String]): Unit = print(5771)
}
