package org.podval.calendar.angles

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import Angles.Rotation

class RotationTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  behavior of "Rotation"

  it should "construct correctly" in {
    def construction(degrees: Int, minutes: Int): Unit = {
      val angle = Rotation(degrees, minutes)
      angle.degrees shouldBe degrees
      angle.minutes shouldBe minutes
    }

    construction(  5, 34)
    construction( 54, 34)
    construction(154, 59)
    construction(254,  0)
  }

  it should "convert correctly" in {
    def conversion(degrees: Int, minutes: Int): Unit = {
      val angle = Rotation(degrees, minutes)
      Rotation.fromDegrees(angle.toDegrees, 2) shouldBe angle
    }

    conversion(5, 34)
    conversion(54, 34)
    conversion(154, 59)
    conversion(254, 0)
  }

  it should "round correctly" in {
    Rotation(104, 58, 50, 16, 39, 59, 43).roundToSeconds shouldBe Rotation(104, 58, 50)
    (Rotation(0, 15, 15)*2).roundToMinutes shouldBe Rotation(0, 31)
    (-Rotation(182, 29, 37)).roundToMinutes shouldBe -Rotation(182, 30)
  }

  it should "compare correctly" in {
    Rotation(0, 0, 0) == Rotation(-0) shouldBe true
    Rotation(15) == Rotation(15) shouldBe true
    Rotation(15) == Rotation(-345).canonical shouldBe true
  }

  it should "normalize and canonicalize correctly" in {
    Rotation(345).normal shouldBe Rotation(345)
    Rotation(-15).normal shouldBe Rotation(-15)
    Rotation(721).normal shouldBe Rotation(1)
    Rotation(-721).normal shouldBe Rotation(-1)
    -Rotation(360, 49, 59, 60).normal shouldBe -Rotation(0, 50)

    Rotation(345).canonical shouldBe Rotation(345)
    Rotation(-15).canonical shouldBe Rotation(345)
    Rotation(721).canonical shouldBe Rotation(1)
    Rotation(-721).canonical shouldBe Rotation(359)

    Rotation(345).symmetrical shouldBe Rotation(-15)
    Rotation(-15).symmetrical shouldBe Rotation(-15)
    Rotation(721).symmetrical shouldBe Rotation(1)
    Rotation(-721).symmetrical shouldBe Rotation(-1)
  }

  it should "negate correctly" in {
    -Rotation(3) shouldBe Rotation(-3)
    -Rotation(0, 3) shouldBe Rotation(0, -3)
  }

  it should "add and subtract correctly" in {
    Rotation( 30) + Rotation(  0) shouldBe Rotation( 30)
    Rotation(  0) + Rotation( 30) shouldBe Rotation( 30)
    Rotation(-30) + Rotation(  0) shouldBe Rotation(-30)
    Rotation(  0) + Rotation(-30) shouldBe Rotation(-30)
    Rotation( 30) + Rotation(-30) shouldBe Rotation(  0)
    Rotation(-30) + Rotation( 30) shouldBe Rotation(  0)

    Rotation( 30) - Rotation(  0) shouldBe Rotation( 30)
    Rotation(  0) - Rotation( 30) shouldBe Rotation(-30)
    Rotation(-30) - Rotation(  0) shouldBe Rotation(-30)
    Rotation(  0) - Rotation(-30) shouldBe Rotation( 30)
    Rotation( 30) - Rotation(-30) shouldBe Rotation( 60)
    Rotation(-30) - Rotation( 30) shouldBe Rotation(-60)
  }

  "angles" should "multiply correctly" in {
    Rotation(90)*2 shouldBe Rotation(180)
    Rotation(90)*3 shouldBe Rotation(270)
    Rotation(90)*4 shouldBe Rotation(360)
    Rotation(90)*5 shouldBe Rotation(450)
  }
}
