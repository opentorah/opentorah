package org.opentorah.xml

import zio.ZIO

trait Conversion[A] {
  def toString(value: A): String = value.toString

  def fromString(value: String): A

  // TODO handle exceptions?
  def parseFromString: String => Parser[A] = value => ZIO.succeed(fromString(value))
}

object Conversion {

  trait StringConversion extends Conversion[String] {
    final override def fromString(value: String): String = value

    final override def parseFromString: String => Parser[String] = ZIO.succeed(_)
  }

  trait BooleanConversion extends Conversion[Boolean] {
    final override def fromString(value: String): Boolean = value.toBoolean

    final override def parseFromString: String => Parser[Boolean] = String2.boolean
  }

  trait IntConversion extends Conversion[Int] {
    final override def fromString(value: String): Int = value.toInt

    final override def parseFromString: String => Parser[Int] = String2.int
  }

  trait PositiveIntConversion extends Conversion[Int] {
    final override def fromString(value: String): Int = {
      val result = value.toInt
      if (result <= 0) throw new IllegalArgumentException(s"Non-positive integer: $result")
      result
    }

    final override def parseFromString: String => Parser[Int] = String2.positiveInt
  }

  trait FloatConversion extends Conversion[Float] {
    final override def fromString(value: String): Float = value.toFloat
  }
}