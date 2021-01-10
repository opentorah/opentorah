package org.opentorah.metadata

import org.opentorah.xml.{Attribute, Parser, Result}

final class WithNumber[T](val n: Int, val what: T)

object WithNumber {

  val nAttribute: Attribute.Required[Int] = new Attribute.PositiveIntAttribute("n").required

  def parse[T](parser: Parser[T]): Parser[WithNumber[T]] = for {
    n <- nAttribute()
    what <- parser
  } yield new WithNumber[T](n, what)

  def checkConsecutive[T](result: Seq[WithNumber[T]], what: String): Result =
    Parser.check(result.map(_.n) == (1 to result.length), s"Wrong $what numbers: $result")

  def checkNumber[T](result: Seq[WithNumber[T]], number: Int, what: String): Result = for {
    _ <- checkConsecutive(result, what)
    _ <- Parser.check(result.length == number, s"Wrong number of ${what}s: ${result.length} != $number")
  } yield ()

  def overlay[T](base: Seq[WithNumber[T]], differences: Seq[WithNumber[T]]): Seq[WithNumber[T]] = {
    val result = scala.collection.mutable.ArrayBuffer.empty[WithNumber[T]] ++= base
    differences.foreach(value => result(value.n - 1) = value)
    result.toSeq
  }

  def dropNumbers[T](result: Seq[WithNumber[T]]): Seq[T] = result.map(_.what)
}
