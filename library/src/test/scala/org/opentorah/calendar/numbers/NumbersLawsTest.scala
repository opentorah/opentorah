package org.opentorah.calendar.numbers

import cats.Eq
import org.scalatest.funsuite.AnyFunSuite
import algebra.CommutativeGroup
import algebra.laws.GroupLaws
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class NumbersLawsTest[N <: Numbers[N]](numberSystem: N) extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  val headRange: Int = numberSystem.headRangeOpt.getOrElse(Int.MaxValue)
  val ranges: Seq[Int] = headRange +: numberSystem.ranges

  def digits: Gen[Seq[Int]] = Gen.sequence[Seq[Int], Int](
    ranges.map { range: Int => Gen.choose[Int](0, range-1) }
  )

  def vectors: Gen[N#Vector] = for { d <- digits } yield { numberSystem.Vector.fromDigits(d) }

  implicit def isArbitrary: Arbitrary[N#Vector] = Arbitrary(vectors)

  implicit val isCommutativeGroup: CommutativeGroup[N#Vector] = new CommutativeGroup[N#Vector] {
    override def empty: N#Vector = numberSystem.Vector.zero
    override def combine(x: N#Vector, y: N#Vector): N#Vector = x + y
    override def inverse(a: N#Vector): N#Vector = -a
  }

  implicit val isEq: Eq[N#Vector] = Eq.fromUniversalEquals

  checkAll("NumberSystem.CommutativeGroupLaws", GroupLaws[N#Vector].commutativeGroup)
}
