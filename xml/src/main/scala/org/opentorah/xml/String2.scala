package org.opentorah.xml

import zio.ZIO

// TODO dissolve into Conversion
object String2 {

  val boolean: String => Parser[Boolean] =
    string => ZIO.succeed(string == "true" || string == "yes")

  val int: String => Parser[Int] =
    int(mustBePositive = false)

  val positiveInt: String => Parser[Int] =
    int(mustBePositive = true)

  private def int(mustBePositive: Boolean): String => Parser[Int] = string => for {
    result <- Parser.effect(string.toInt)
    _ <- Parser.check(!mustBePositive || result > 0, s"Non-positive integer: $result")
  } yield result
}
