package org.opentorah.numbers

trait DigitsDescriptor:
  open class DigitBase(override val sign: String) extends DigitsDescriptor.Digit:
    final override def position: Int = values.indexOf(this)

  val values: Seq[DigitsDescriptor.Digit]

  lazy val signs: Seq[String] = values.map(_.sign)

  def forPosition(index: Int): DigitsDescriptor.Digit = values(index)

  final def length: Int = values.length

object DigitsDescriptor:

  // TODO move into Numbers?
  trait Digit:
    def sign: String

    def position: Int