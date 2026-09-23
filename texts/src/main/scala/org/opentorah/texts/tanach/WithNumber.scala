package org.opentorah.texts.tanach

import org.opentorah.util.Collections
import org.podval.xml.Xml
import Xml.given

final class WithNumber[T](val n: Int, val what: T)

object WithNumber:

  def decode[T](element: Xml.Element, what: Xml.Element => T): WithNumber[T] =
    WithNumber(element.positiveInt("n"), what(element))

  def requireConsecutive[T](result: Seq[WithNumber[T]], what: String): Unit =
    Collections.requireConsecutive(result, _.n, what)

  def requireNumber[T](result: Seq[WithNumber[T]], number: Int, what: String): Unit =
    Collections.requireConsecutive(result, _.n, what, count = Some(number))

  def overlay[T](base: Seq[WithNumber[T]], differences: Seq[WithNumber[T]]): Seq[WithNumber[T]] =
    val result = scala.collection.mutable.ArrayBuffer.empty[WithNumber[T]] ++= base
    differences.foreach(value => result(value.n - 1) = value)
    result.toSeq

  def dropNumbers[T](result: Seq[WithNumber[T]]): Seq[T] = result.map(_.what)
