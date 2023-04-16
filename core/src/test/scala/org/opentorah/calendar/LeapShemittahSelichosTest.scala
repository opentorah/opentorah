package org.opentorah.calendar

import org.opentorah.calendar.jewish.Jewish.Year
import org.opentorah.calendar.jewish.{NewYear, SpecialDay}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class LeapShemittahSelichosTest extends AnyFlatSpec, Matchers:
  "Shemittah/Leap/Long Slichos frequencies" should "be known" in:
    var num: Int = 0
    var numLeap: Int = 0
    var numShemittah: Int = 0
    var numLeapShemittah: Int = 0
    val numDaysSelichosArray: Array[Int] = Array.fill(15)(0)

    for number <- NewYear.delaysEnabledFromYear to 6000 do
      val year = Year(number)
      num += 1

      val isLeap: Boolean = year.isLeap
      val isShemittah = year.isShemittah
      val numDaysSelichos: Int = SpecialDay.numDaysSelichos(year)
      numDaysSelichosArray(numDaysSelichos) += 1

      if isLeap then numLeap += 1
      if isShemittah then numShemittah += 1
      if isLeap && isShemittah then numLeapShemittah += 1

    println(s"all=$num; leap=$numLeap; shemittah=$numShemittah; leap shemittah=$numLeapShemittah")
    println(s"numDaysSelichos=${numDaysSelichosArray.mkString(", ")}")
