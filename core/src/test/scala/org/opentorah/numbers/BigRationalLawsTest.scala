package org.opentorah.numbers

import cats.Eq
// TODO if Field and Ring ever make it into cats, we can drop the algebraLaws dependency:
import algebra.ring.{AdditiveMonoid, Field} // not in cats.kernel      as of 2.7.0
import algebra.laws.RingLaws                // not in cats.kernel.laws as of 2.7.0
import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.Arbitrary
import org.scalactic.anyvals.PosZDouble
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class BigRationalLawsTest extends AnyFunSuite, FunSuiteDiscipline, Checkers:
  import BigRationalTest.*

  // we discard zero denominators, so we need a higher maxDiscardedFactor than the default 5.0:
  // TODO this is the last implicit (as opposed to given) remaining in the code; eventually ScalaTest/Check/whatever will update...
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(maxDiscardedFactor = PosZDouble(70.0))

  given isArbitrary: Arbitrary[BigRational] = Arbitrary(rational)

  given isEq: Eq[BigRational] = Eq.fromUniversalEquals

  given AdditiveMonoid[BigRational] with
    override def zero: BigRational = BigRational.zero
    override def plus(x: BigRational, y: BigRational): BigRational = x + y

  given Field[BigRational] with
    override def zero: BigRational = BigRational.zero
    override def plus(x: BigRational, y: BigRational): BigRational = x + y
    override def negate(x: BigRational): BigRational = - x
    override def one: BigRational = BigRational.one
    override def times(x: BigRational, y: BigRational): BigRational = x * y
    override def div(x: BigRational, y: BigRational): BigRational = x / y

  checkAll("BigRational.FieldLaws", RingLaws[BigRational].field)
