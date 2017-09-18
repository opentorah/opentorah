package org.podval.calendar.angle

import org.scalatest.FlatSpec

import AngleNumberSystem.Angle

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class AngleTest extends FlatSpec {

  behavior of "Angle"

  it should "construct correctly" in {
    def construction(degrees: Int, minutes: Int) {
      val angle = Angle(degrees, minutes)
      assertResult(degrees)(angle.degrees)
      assertResult(minutes)(angle.minutes)
    }

    construction(  5, 34)
    construction( 54, 34)
    construction(154, 59)
    construction(254,  0)
  }

  it should "convert correctly" in {
    def conversion(degrees: Int, minutes: Int) {
      val angle = Angle(degrees, minutes)
      assert(angle == Angle.fromDegrees(angle.toDegrees, 2))
    }

    conversion(5, 34)
    conversion(54, 34)
    conversion(154, 59)
    conversion(254, 0)
  }

  it should "round correctly" in {
    assertResult(Angle(104, 58, 50))(Angle(104, 58, 50, 16, 39, 59, 43).roundToSeconds)
    assertResult(Angle(0, 31))((Angle(0, 15, 15)*2).roundToMinutes)
  }

  it should "compare correctly" in {
    assertResult(Angle(0, 0, 0))(Angle(-0))
    assertResult(Angle(15))(Angle(15))
    assertResult(Angle(15))(Angle(-345))
  }

  it should "normalize and canonicalize correctly" in {
    assertResult(Angle(345))(Angle(345).normal)
    assertResult(Angle(-15))(Angle(-15).normal)
    assertResult(Angle(1))(Angle(721).normal)
    assertResult(Angle(-1))(Angle(-721).normal)
    assertResult(-Angle(0, 50))(-Angle(360, 49, 59, 60).normal)

    assertResult(Angle(345))(Angle(345).canonical)
    assertResult(Angle(345))(Angle(-15).canonical)
    assertResult(Angle(1))(Angle(721).canonical)
    assertResult(Angle(359))(Angle(-721).canonical)

    assertResult(Angle(-15))(Angle(345).symmetrical)
    assertResult(Angle(-15))(Angle(-15).symmetrical)
    assertResult(Angle(1))(Angle(721).symmetrical)
    assertResult(Angle(-1))(Angle(-721).symmetrical)
  }

  it should "negate correctly" in {
    assertResult(Angle(-3))(-Angle(3))
    assertResult(Angle(0, -3))(-Angle(0, 3))
  }

  // TODO move into TimeNumberSystemTest
  it should "add and subtract correctly" in {
    assertResult(Angle( 30))(Angle( 30) + Angle(  0))
    assertResult(Angle( 30))(Angle(  0) + Angle( 30))
    assertResult(Angle(-30))(Angle(-30) + Angle(  0))
    assertResult(Angle(-30))(Angle(  0) + Angle(-30))
    assertResult(Angle(  0))(Angle( 30) + Angle(-30))
    assertResult(Angle(  0))(Angle(-30) + Angle( 30))

    assertResult(Angle( 30))(Angle( 30) - Angle(  0))
    assertResult(Angle(-30))(Angle(  0) - Angle( 30))
    assertResult(Angle(-30))(Angle(-30) - Angle(  0))
    assertResult(Angle( 30))(Angle(  0) - Angle(-30))
    assertResult(Angle( 60))(Angle( 30) - Angle(-30))
    assertResult(Angle(-60))(Angle(-30) - Angle( 30))
  }
}
