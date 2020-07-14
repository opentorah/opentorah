package org.opentorah.xml

import zio.ZIO

trait Conversion[A] {
  def toString(value: A): String = value.toString

  def fromString(value: String): A

  def parseFromString(value: String): Parser[A] = for {
    result <- Parser.effect(fromString(value))
  } yield result
}

object Conversion {

  trait StringConversion extends Conversion[String] {
    final override def fromString(value: String): String = value

    final override def parseFromString(value: String): Parser[String] = ZIO.succeed(value)
  }

  trait BooleanConversion extends Conversion[Boolean] {
    final override def fromString(value: String): Boolean = (value == "yes") || value.toBoolean
  }

  trait IntConversion extends Conversion[Int] {
    final override def fromString(value: String): Int = int(value, mustBePositive = false)
  }

  trait PositiveIntConversion extends Conversion[Int] {
    final override def fromString(value: String): Int = int(value, mustBePositive = true)
  }

  trait FloatConversion extends Conversion[Float] {
    final override def fromString(value: String): Float = value.toFloat
  }

  private def int(value: String, mustBePositive: Boolean): Int = {
    val result = value.toInt
    if (mustBePositive && result <= 0) throw new IllegalArgumentException(s"Non-positive integer: $result")
    result
  }
}