package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle
import Angle2Angle.Table

abstract class Angle2Angle {
  val table: Table
}


object Angle2Angle {
  trait Table {
    def calculate(moonAnomalyTrue: Angle): Angle

    def a0  : Angle = Angle.zero
    def a10 : Angle
    def a20 : Angle
    def a30 : Angle
    def a40 : Angle
    def a50 : Angle
    def a60 : Angle
    def a70 : Angle
    def a80 : Angle
    def a90 : Angle
    def a100: Angle
    def a110: Angle
    def a120: Angle
    def a130: Angle
    def a140: Angle
    def a150: Angle
    def a160: Angle
    def a170: Angle
    def a180: Angle = Angle.zero

    // KH 13:7-8, 15:7
    final def interpolate(angle: Angle): Angle = {
      val (before: Angle, after: Angle) =
        if ((Angle(  0) <= angle) && (angle < Angle( 10))) (  a0,  a10) else
        if ((Angle( 10) <= angle) && (angle < Angle( 20))) ( a10,  a20) else
        if ((Angle( 20) <= angle) && (angle < Angle( 30))) ( a20,  a30) else
        if ((Angle( 30) <= angle) && (angle < Angle( 40))) ( a30,  a40) else
        if ((Angle( 40) <= angle) && (angle < Angle( 50))) ( a40,  a50) else
        if ((Angle( 50) <= angle) && (angle < Angle( 60))) ( a50,  a60) else
        if ((Angle( 60) <= angle) && (angle < Angle( 70))) ( a60,  a70) else
        if ((Angle( 70) <= angle) && (angle < Angle( 80))) ( a70,  a80) else
        if ((Angle( 80) <= angle) && (angle < Angle( 90))) ( a80,  a90) else
        if ((Angle( 90) <= angle) && (angle < Angle(100))) ( a90, a100) else
        if ((Angle(100) <= angle) && (angle < Angle(110))) (a100, a110) else
        if ((Angle(110) <= angle) && (angle < Angle(120))) (a110, a120) else
        if ((Angle(120) <= angle) && (angle < Angle(130))) (a120, a130) else
        if ((Angle(130) <= angle) && (angle < Angle(140))) (a130, a140) else
        if ((Angle(140) <= angle) && (angle < Angle(150))) (a140, a150) else
        if ((Angle(150) <= angle) && (angle < Angle(160))) (a150, a160) else
        if ((Angle(160) <= angle) && (angle < Angle(170))) (a160, a170) else
        if ((Angle(170) <= angle) && (angle < Angle(180))) (a170, a180) else
          throw new IllegalArgumentException

      val reminder: Angle = angle.head(angle.head % 10)
      val more: Angle = (after-before)*(reminder/10)
      before + more
    }
  }

  final val keys: Seq[Angle] = (0 to 18).map(_ * 10).map(Angle(_))
}
