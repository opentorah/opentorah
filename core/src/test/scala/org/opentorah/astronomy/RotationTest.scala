package org.opentorah.astronomy

import Angles.{Position, Rotation}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class RotationTest extends AnyFlatSpec, ScalaCheckDrivenPropertyChecks, Matchers:

  behavior of "Rotation"

  it should "construct correctly" in {
    def construction(degrees: Int, minutes: Int): Unit =
      val angle = Rotation(degrees, minutes)
      angle.degrees shouldBe degrees
      angle.minutes shouldBe minutes

    construction(  5, 34)
    construction( 54, 34)
    construction(154, 59)
    construction(254,  0)
  }

  it should "convert correctly" in {
    def conversion(value: String): Unit =
      val angle: Rotation = Rotation(value)
      Rotation.fromDegrees(angle.toDegrees, 2) shouldBe angle

    conversion("  5°34′")
    conversion(" 54°34′")
    conversion("154°59′")
    conversion("254° 0′")
  }

  it should "round correctly" in {
    Rotation("104°58′50″16‴39,59,43").roundToSeconds shouldBe Rotation("104°58′50″")
    (Rotation("0°15′15″")*2).roundToMinutes shouldBe Rotation("0°31′")
    Rotation("-182°29′37″").roundToMinutes shouldBe Rotation("-182°30′")
  }

  it should "compare correctly" in {
    Rotation("0°0′0″") shouldBe Rotation("-0°")
    Rotation("15°") shouldBe Rotation("15°")
    Rotation("-345°").canonical shouldBe Rotation("15°")
    Rotation("345°") shouldBe Rotation("345°")
    Rotation("-15°") shouldBe Rotation("-15°")

    Position("721°") shouldBe Position("1°")
    Rotation("721°") == Rotation("1°") shouldBe false
    Rotation("721°").canonical shouldBe Rotation("1°")
    Rotation("-721°").canonical shouldBe Rotation("-1°").canonical
    -Rotation("360°49′59″60‴").canonical shouldBe -Rotation("0°50′")

    Rotation("30°") > Rotation("15°") shouldBe true
  }

  it should "canonicalize correctly" in {
    Rotation("51°").digits shouldBe List(51)
    Rotation("51°").canonical.digits shouldBe List(51)

    Rotation("-51°").digits shouldBe List(-51)
    Rotation("-51°").canonical.digits shouldBe List(309)

    (Rotation("360°") - Rotation("-51°")).digits shouldBe List(411)
    (Rotation("360°") - Rotation("-51°").canonical).digits shouldBe List(51)

    def canonical(value: String, result: String): Unit =
      Rotation(value).canonical shouldBe Rotation(result)

    canonical(" 345°", "345°")
    canonical("- 15°", "345°")
    canonical(" 721°", "  1°")
    canonical("-721°", "359°")
  }

  it should "negate correctly" in {
    -Rotation(3) shouldBe Rotation(-3)
    -Rotation(0, 3) shouldBe Rotation(0, -3)
  }

  it should "add and subtract correctly" in {
    def add(left: String, right: String, result: String): Unit =
      Rotation(left) + Rotation(right) shouldBe Rotation(result)

    add(" 30°", "  0°", " 30°")
    add("  0°", " 30°", " 30°")
    add("-30°", "  0°", "-30°")
    add("  0°", "-30°", "-30°")
    add(" 30°", "-30°", "  0°")
    add("-30°", " 30°", "  0°")

    def sub(left: String, right: String, result: String): Unit =
      Rotation(left) - Rotation(right) shouldBe Rotation(result)

    sub(" 30°", "  0°", " 30°")
    sub("  0°", " 30°", "-30°")
    sub("-30°", "  0°", "-30°")
    sub("  0°", "-30°", " 30°")
    sub(" 30°", "-30°", " 60°")
    sub("-30°", " 30°", "-60°")
  }

  it should "multiply by number correctly" in {
    def check(value: String, multiplier: Int, result: String): Unit =
      Rotation(value)*multiplier shouldBe Rotation(result)

    check("90°", 2, "180°")
    check("90°", 3, "270°")
    check("90°", 4, "360°")
    check("90°", 5, "450°")
  }
