package org.podval.judaica.metadata.tanach

import scala.collection.mutable.ArrayBuffer

trait WithNumber {
  def n: Int
}

object WithNumber {
  def checkConsecutive[T <: WithNumber](result: Seq[T], what: String): Seq[T] = {
    require(result.map(_.n) == (1 to result.length), s"Wrong $what numbers: $result")
    result
  }

  def checkNumber[T <: WithNumber](result: Seq[T], number: Int, what: String): Seq[T] = {
    checkConsecutive(result, what)
    require(result.length == number, s"Wrong number of ${what}s")
    result
  }

  def overlay[T <: WithNumber](base: Seq[T], differences: Seq[T]): Seq[T] = {
    val result: ArrayBuffer[T] = ArrayBuffer.empty[T] ++= base
    differences.foreach(value => result(value.n - 1) = value)
    result
  }
}
