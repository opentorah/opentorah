package org.podval.calendar.astronomy

import org.scalatest.FlatSpec
import org.podval.calendar.jewish.Jewish.Year

class SeasonsTest extends FlatSpec {
  "tkufas Nisan" should "calculate correctly" in {
    val seasons = new SeasonsAstronomical(Calculator.Text)
//    println(seasons.tkufasNisan(Year(5778)))
  }
}
