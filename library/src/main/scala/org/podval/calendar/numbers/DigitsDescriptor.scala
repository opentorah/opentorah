package org.podval.calendar.numbers

import DigitsDescriptor.Digit

trait DigitsDescriptor {
  class DigitBase(override val sign: String) extends Digit {
    final override def position: Int = values.indexOf(this)
  }

  val values: Seq[Digit]

  def position(digit: Digit): Int = values.indexOf(digit)

  def forPosition(index: Int): Digit = values(index)

  final def length: Int = values.length
}


object DigitsDescriptor {

  trait Digit {
    def sign: String

    def position: Int
  }
}
