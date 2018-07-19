package org.podval.calendar.angle

import org.scalatest.FlatSpec

import Angles.Rotation

class AngleTest extends FlatSpec {

  behavior of "Angle"

  it should "construct correctly" in {
    def construction(degrees: Int, minutes: Int) {
      val angle = Rotation(degrees, minutes)
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
      val angle = Rotation(degrees, minutes)
      assert(angle == Rotation.fromDegrees(angle.toDegrees, 2))
    }

    conversion(5, 34)
    conversion(54, 34)
    conversion(154, 59)
    conversion(254, 0)
  }

  it should "round correctly" in {
    assertResult(Rotation(104, 58, 50))(Rotation(104, 58, 50, 16, 39, 59, 43).roundToSeconds)
    assertResult(Rotation(0, 31))((Rotation(0, 15, 15)*2).roundToMinutes)
    assertResult(-Rotation(182, 30))((-Rotation(182, 29, 37)).roundToMinutes)
  }

  it should "compare correctly" in {
    assertResult(Rotation(0, 0, 0))(Rotation(-0))
    assertResult(Rotation(15))(Rotation(15))
    assertResult(Rotation(15))(Rotation(-345).canonical)
  }

  it should "normalize and canonicalize correctly" in {
    assertResult(Rotation(345))(Rotation(345).normal)
    assertResult(Rotation(-15))(Rotation(-15).normal)
    assertResult(Rotation(1))(Rotation(721).normal)
    assertResult(Rotation(-1))(Rotation(-721).normal)
    assertResult(-Rotation(0, 50))(-Rotation(360, 49, 59, 60).normal)

    assertResult(Rotation(345))(Rotation(345).canonical)
    assertResult(Rotation(345))(Rotation(-15).canonical)
    assertResult(Rotation(1))(Rotation(721).canonical)
    assertResult(Rotation(359))(Rotation(-721).canonical)

    assertResult(Rotation(-15))(Rotation(345).symmetrical)
    assertResult(Rotation(-15))(Rotation(-15).symmetrical)
    assertResult(Rotation(1))(Rotation(721).symmetrical)
    assertResult(Rotation(-1))(Rotation(-721).symmetrical)
  }

  it should "negate correctly" in {
    assertResult(Rotation(-3))(-Rotation(3))
    assertResult(Rotation(0, -3))(-Rotation(0, 3))
  }

  it should "add and subtract correctly" in {
    assertResult(Rotation( 30))(Rotation( 30) + Rotation(  0))
    assertResult(Rotation( 30))(Rotation(  0) + Rotation( 30))
    assertResult(Rotation(-30))(Rotation(-30) + Rotation(  0))
    assertResult(Rotation(-30))(Rotation(  0) + Rotation(-30))
    assertResult(Rotation(  0))(Rotation( 30) + Rotation(-30))
    assertResult(Rotation(  0))(Rotation(-30) + Rotation( 30))

    assertResult(Rotation( 30))(Rotation( 30) - Rotation(  0))
    assertResult(Rotation(-30))(Rotation(  0) - Rotation( 30))
    assertResult(Rotation(-30))(Rotation(-30) - Rotation(  0))
    assertResult(Rotation( 30))(Rotation(  0) - Rotation(-30))
    assertResult(Rotation( 60))(Rotation( 30) - Rotation(-30))
    assertResult(Rotation(-60))(Rotation(-30) - Rotation( 30))
  }

  "angles" should "multiply correctly" in {
    assertResult(Rotation(180))(Rotation(90)*2)
    assertResult(Rotation(270))(Rotation(90)*3)
    assertResult(Rotation(360))(Rotation(90)*4)
    assertResult(Rotation(450))(Rotation(90)*5)
  }
}
