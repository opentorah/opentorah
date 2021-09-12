package org.opentorah.metadata

import org.opentorah.util.Effects
import org.opentorah.xml.{Attribute, Parser}
import zio.IO

final class WithNumber[T](val n: Int, val what: T)

object WithNumber:

  val nAttribute: Attribute.Required[Int] = Attribute.PositiveIntAttribute("n").required

  def parse[T](parser: Parser[T]): Parser[WithNumber[T]] = for
    n: Int <- nAttribute()
    what: T <- parser
  yield WithNumber[T](n, what)

  def checkConsecutive[T](result: Seq[WithNumber[T]], what: String): IO[Effects.Error, Unit] =
    Effects.check(result.map(_.n) == (1 to result.length), s"Wrong $what numbers: $result")

  def checkNumber[T](result: Seq[WithNumber[T]], number: Int, what: String): IO[Effects.Error, Unit] = for
    _ <- checkConsecutive(result, what)
    _ <- Effects.check(result.length == number, s"Wrong number of ${what}s: ${result.length} != $number")
  yield ()

  def overlay[T](base: Seq[WithNumber[T]], differences: Seq[WithNumber[T]]): Seq[WithNumber[T]] =
    val result = scala.collection.mutable.ArrayBuffer.empty[WithNumber[T]] ++= base
    differences.foreach(value => result(value.n - 1) = value)
    result.toSeq

  def dropNumbers[T](result: Seq[WithNumber[T]]): Seq[T] = result.map(_.what)
