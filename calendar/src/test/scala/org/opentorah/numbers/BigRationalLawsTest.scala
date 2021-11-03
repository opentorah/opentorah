package org.opentorah.numbers

import cats.Eq
import algebra.ring.{AdditiveMonoid, Field}
import algebra.laws.RingLaws
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

  given isEq: Eq[BigRational] = Eq.fromUniversalEquals

  checkAll("BigRational.FieldLaws", RingLaws[BigRational].field)
