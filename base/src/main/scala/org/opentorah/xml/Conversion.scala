package org.opentorah.xml

import org.opentorah.util.Effects
import zio.IO

trait Conversion[A]:
  def toString(value: A): String = value.toString

  final def get(value: Option[String]): Option[A] =
    value.filter(_.nonEmpty).map(fromString)

  def fromString(value: String): A

  def parseFromString(value: String): IO[Effects.Error, A] = Effects.effect(fromString(value)) // TODO ZIOify!

object Conversion:

  trait StringConversion extends Conversion[String]:
    final override def fromString(value: String): String = value

    final override def parseFromString(value: String): IO[Effects.Error, String] = IO.succeed(value)

  trait BooleanConversion extends Conversion[Boolean]:
    final override def fromString(value: String): Boolean = value match
      case "yes" => true
      case "no"  => false
      case value => value.toBoolean

  trait IntConversion extends Conversion[Int]:
    final override def fromString(value: String): Int = int(value, mustBePositive = false)

  trait PositiveIntConversion extends Conversion[Int]:
    final override def fromString(value: String): Int = int(value, mustBePositive = true)

  trait FloatConversion extends Conversion[Float]:
    final override def fromString(value: String): Float = value.toFloat

  private def int(value: String, mustBePositive: Boolean): Int =
    val result = value.toInt
    if mustBePositive && result <= 0 then throw IllegalArgumentException(s"Non-positive integer: $result")
    result
