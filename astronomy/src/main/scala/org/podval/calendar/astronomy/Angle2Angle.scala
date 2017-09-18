package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle
import Angle2Angle.Table

abstract class Angle2Angle {
  val table: Table

  // KH 13:7-8, 15:7
  final def interpolate(table: Table)(angle: Angle): Angle = {
    val (before: Angle, after: Angle) =
      if ((Angle(  0) <= angle) && (angle < Angle( 10))) (table.  a0, table. a10) else
      if ((Angle( 10) <= angle) && (angle < Angle( 20))) (table. a10, table. a20) else
      if ((Angle( 20) <= angle) && (angle < Angle( 30))) (table. a20, table. a30) else
      if ((Angle( 30) <= angle) && (angle < Angle( 40))) (table. a30, table. a40) else
      if ((Angle( 40) <= angle) && (angle < Angle( 50))) (table. a40, table. a50) else
      if ((Angle( 50) <= angle) && (angle < Angle( 60))) (table. a50, table. a60) else
      if ((Angle( 60) <= angle) && (angle < Angle( 70))) (table. a60, table. a70) else
      if ((Angle( 70) <= angle) && (angle < Angle( 80))) (table. a70, table. a80) else
      if ((Angle( 80) <= angle) && (angle < Angle( 90))) (table. a80, table. a90) else
      if ((Angle( 90) <= angle) && (angle < Angle(100))) (table. a90, table.a100) else
      if ((Angle(100) <= angle) && (angle < Angle(110))) (table.a100, table.a110) else
      if ((Angle(110) <= angle) && (angle < Angle(120))) (table.a110, table.a120) else
      if ((Angle(120) <= angle) && (angle < Angle(130))) (table.a120, table.a130) else
      if ((Angle(130) <= angle) && (angle < Angle(140))) (table.a130, table.a140) else
      if ((Angle(140) <= angle) && (angle < Angle(150))) (table.a140, table.a150) else
      if ((Angle(150) <= angle) && (angle < Angle(160))) (table.a150, table.a160) else
      if ((Angle(160) <= angle) && (angle < Angle(170))) (table.a160, table.a170) else
      if ((Angle(170) <= angle) && (angle < Angle(180))) (table.a170, table.a180) else
        throw new IllegalArgumentException

    val reminder: Angle = angle.head(angle.head % 10)
    val more: Angle = (after-before)*(reminder/10)
    before + more
  }

  // KH 13:?; 15:4, 15:7
  final def fromTable(table: Table)(rawAngle: Angle): Angle = {
    // TODO introduce roundToDegrees()
    // TODO round for MoonAnomalyVisible too?
    val angle: Angle = rawAngle.roundTo(0) // KH 13:9
    if (angle < Angle(180)) -interpolate(table)(angle) else
    if (angle > Angle(180))  interpolate(table)(angle - Angle(180)) else
      Angle(0)
  }

  final def fromTable(angle: Angle): Angle = fromTable(table)(angle)
}


object Angle2Angle {
  trait Table {
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
  }

  final val keys: Seq[Angle] = (0 to 18).map(_ * 10).map(Angle(_))
}
