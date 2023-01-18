package org.opentorah.astronomy

import Days2Rotation.Days
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Days2RotationTest(val what: Days2Rotation) extends AnyFlatSpec, Matchers
//  def multiply(days: Days, n: Int): Angles.Rotation = (what.value(days)*n).canonical
//
//  def tooMuch(from: Days, multiplier: Int, to: Days): Unit =
//    multiply(from, multiplier) should be > what.value(to)
//
//  def theSame(from: Days, multiplier: Int, to: Days): Unit =
//    multiply(from, multiplier) should be (what.value(to))

