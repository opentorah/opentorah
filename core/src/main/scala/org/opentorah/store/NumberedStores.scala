package org.opentorah.store

import org.opentorah.metadata.{Language, Name, Names}
import org.opentorah.xml.Parser
import zio.ZIO

// TODO override indexOf()
trait NumberedStores[+T <: NumberedStore] extends Pure[T]:
  def minNumber: Int = 1
  // Derived types must override one of the:
  def maxNumber: Int = minNumber + length - 1
  def length: Int = maxNumber - minNumber + 1

  final def contains(number: Int): Boolean = minNumber <= number && number <= maxNumber

  def name2number(name: String): Option[Int] =
    name.toIntOption.orElse(Language.Hebrew.numberFromString(name))

  def number2names(number: Int): Names = new Names(Seq(
    Name(number.toString, Language.Spec.empty),
    Name(Language.Hebrew.numberToString(number), Language.Hebrew.toSpec)
  ))

  override def storesPure: Seq[T] = minNumber.to(maxNumber).map(createNumberedStore)

  protected def createNumberedStore(number: Int): T

  final override def findByName(name: String): Parser[Option[T]] =
    ZIO.succeed(name2number(name).filter(contains).map(createNumberedStore))