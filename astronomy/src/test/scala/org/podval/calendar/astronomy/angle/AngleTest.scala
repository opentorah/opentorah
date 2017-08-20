package org.podval.calendar.astronomy.angle

import org.scalatest.FlatSpec

import AngleNumberSystem.Angle


@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class AngleTest extends FlatSpec {

    behavior of "Angle"

    it should "construct properly" in {

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


    it should "convert properly" in {

      def conversion(degrees: Int, minutes: Int) {
        val angle = Angle(degrees, minutes)
        val value = angle.toDegrees
        val angle_ = Angle.fromDegrees(value, 2)
        assert(angle == angle_)
      }

      conversion(5, 34)
      conversion(54, 34)
      conversion(154, 59)
      conversion(254, 0)
    }


    it should "round properly" in {
      assertResult(Angle(104, 58, 50))(Angle(104, 58, 50, 16, 39, 59, 43).roundToSeconds)

      val a: Angle = Angle(0, 15, 15)
      val a2: Angle = a*2
      assertResult(Angle(0, 31))(a2.roundToMinutes)
    }


  // TODO add tests for negativity
}
