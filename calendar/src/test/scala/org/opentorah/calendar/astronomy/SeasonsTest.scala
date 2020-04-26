package org.opentorah.calendar.astronomy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SeasonsTest extends AnyFlatSpec with Matchers {
  "tkufas Nisan" should "calculate correctly" in {
    val seasons = new SeasonsAstronomical(Calculator.Text)
//    println(seasons.tkufasNisan(Year(5778)))
  }
}
