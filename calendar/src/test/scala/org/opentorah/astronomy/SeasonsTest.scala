package org.opentorah.astronomy

import org.opentorah.calendar.jewish.Season
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SeasonsTest extends AnyFlatSpec, Matchers:
  "Seasons" should "load correctly" in {
    Season.TkufasTishrei.names.hasName("осеннее равноденствие") shouldBe true
  }
  
  "tkufas Nisan" should "calculate correctly" in {
    val seasons = SeasonsAstronomical(Calculator.Text)
//    println(seasons.tkufasNisan(Year(5778)))
  }
