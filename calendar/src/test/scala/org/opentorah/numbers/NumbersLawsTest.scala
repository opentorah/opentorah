package org.opentorah.numbers

import cats.Eq
import org.scalatest.funsuite.AnyFunSuite
import algebra.CommutativeGroup
import algebra.laws.GroupLaws
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

open class NumbersLawsTest(val numbers: Numbers) extends AnyFunSuite, FunSuiteDiscipline, Checkers:
  val headRange: Int = numbers.headRangeOpt.getOrElse(Int.MaxValue)
  val ranges: Seq[Int] = headRange +: numbers.ranges

  def digits: Gen[Seq[Int]] = Gen.sequence[Seq[Int], Int](
    ranges.map((range: Int) => Gen.choose[Int](0, range-1))
  )

  def vectors: Gen[numbers.Vector] = for  d <- digits yield numbers.Vector.fromDigits(d)

  given isArbitrary: Arbitrary[numbers.Vector] = Arbitrary(vectors)

  given CommutativeGroup[numbers.Vector] with
    override def empty: numbers.Vector = numbers.Vector.zero
    override def combine(x: numbers.Vector, y: numbers.Vector): numbers.Vector = x + y
    override def inverse(a: numbers.Vector): numbers.Vector = -a

  given isEq: Eq[numbers.Vector] = Eq.fromUniversalEquals

  checkAll("NumberSystem.CommutativeGroupLaws", GroupLaws[numbers.Vector].commutativeGroup)
