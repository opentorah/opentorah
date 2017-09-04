package org.podval.calendar.astronomy

import org.scalatest.FlatSpec
import org.podval.calendar.angle.AngleNumberSystem
import AngleNumberSystem.{Angle, AnglePoint, headRange, range}
import Zodiac.{Constellation, constellations}

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class TextTest extends FlatSpec {
  "angle units" should "be as in KH 11:7" in {
    assertResult(360)(headRange)
    assertResult( 60)(range(0))
    assertResult( 60)(range(1))
    assertResult( 60)(range(2))
    assertResult( 60)(range(3))
    assertResult( 60)(range(4))
    assertResult( 60)(range(5))
    assertResult( 60)(range(6))
  }

  "zodiac" should "be as in KH 11:7-9" in {
    assertResult(12)(constellations.length)
    assertResult(Angle(0))(Zodiac.Aries.start)
    constellations.init.zip(constellations.tail).foreach {
      case (prev: Constellation, next: Constellation) =>
        assertResult(prev.end)((prev.start + Angle(30)).normal)
        assertResult(next.start)(prev.end)
    }

    val (constellation1: Zodiac.Constellation, angle1: Angle) = Zodiac.fromAngle(AnglePoint(70, 30, 40))
    assertResult(Zodiac.Gemini)(constellation1)
    assertResult(Angle(10, 30, 40))(angle1)

    val (constellation2: Zodiac.Constellation, angle2: Angle) = Zodiac.fromAngle(AnglePoint(320))
    assertResult(Zodiac.Aquarius)(constellation2)
    assertResult(Angle(20))(angle2)
  }

  "angles" should "subtract as in KH 11:12" in {
    // TODO test fails!!!
    assertResult(AnglePoint(259, 29, 50))((AnglePoint(100, 20, 30) - Angle(200, 50, 40)).normal.canonical)
  }
}
