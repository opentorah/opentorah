package org.podval.calendar.jewish

import Jewish.{Day, Month, Year}

// TODO move into a separate package
object NewMoon {
  def print(yearNumber: Int): Unit = {
    val year: Year = Year(yearNumber)
    for { month <- year.months } yield {

    }
  }

  def main(args: Array[String]): Unit = print(5771)
}
