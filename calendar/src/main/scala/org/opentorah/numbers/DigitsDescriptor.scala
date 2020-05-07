package org.opentorah.numbers

trait DigitsDescriptor {
  class DigitBase(override val sign: String) extends Digit {
    final override def position: Int = values.indexOf(this)
  }

  val values: Seq[Digit]

  lazy val signs: Seq[String] = values.map(_.sign)

  def forPosition(index: Int): Digit = values(index)

  final def length: Int = values.length
}
