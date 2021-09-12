package org.opentorah.astronomy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SeasonsTest extends AnyFlatSpec, Matchers:
  "tkufas Nisan" should "calculate correctly" in {
    val seasons = SeasonsAstronomical(Calculator.Text)
//    println(seasons.tkufasNisan(Year(5778)))
  }
