package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.Attributes

final class WithNumber[T](val n: Int, val what: T)

object WithNumber {
  def parse[T](attributes: Attributes, p: Attributes => T): WithNumber[T] = new WithNumber[T](
    n = attributes.doGetInt("n"),
    what = p(attributes)
  )

  def checkConsecutive[T](result: Seq[WithNumber[T]], what: String): Seq[WithNumber[T]] = {
    require(result.map(_.n) == (1 to result.length), s"Wrong $what numbers: $result")
    result
  }

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
