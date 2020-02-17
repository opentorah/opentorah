package org.digitaljudaica.metadata

import cats.implicits._
import org.digitaljudaica.xml.{Parser, Xml}

final class WithNumber[T](val n: Int, val what: T)

object WithNumber {

  def parse[T](parser: Parser[T]): Parser[WithNumber[T]] = for {
    n <- Xml.attribute.required.positiveInt("n")
    what <- parser
  } yield new WithNumber[T](n, what)

  // TODO switch to checkConsecutiveNg?
  def checkConsecutive[T](result: Seq[WithNumber[T]], what: String): Seq[WithNumber[T]] = {
    require(result.map(_.n) == (1 to result.length), s"Wrong $what numbers: $result")
    result
  }

  def checkConsecutiveNg[T](result: Seq[WithNumber[T]], what: String): Parser[Unit] =
    Parser.check(result.map(_.n) == (1 to result.length), s"Wrong $what numbers: $result")

  def checkNumber[T](result: Seq[WithNumber[T]], number: Int, what: String): Seq[WithNumber[T]] = {
    checkConsecutive(result, what)
    require(result.length == number, s"Wrong number of ${what}s: ${result.length} != $number")
    result
  }

  def overlay[T](base: Seq[WithNumber[T]], differences: Seq[WithNumber[T]]): Seq[WithNumber[T]] = {
    val result = scala.collection.mutable.ArrayBuffer.empty[WithNumber[T]] ++= base
    differences.foreach(value => result(value.n - 1) = value)
    result
  }

  def dropNumbers[T](result: Seq[WithNumber[T]]): Seq[T] = result.map(_.what)
}
