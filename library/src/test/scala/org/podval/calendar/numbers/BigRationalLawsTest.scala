package org.podval.calendar.numbers

import cats.Eq
import org.scalatest.FunSuite
import algebra.ring.{AdditiveMonoid, Field}
import algebra.laws.RingLaws
import org.scalacheck.Arbitrary
import org.scalactic.anyvals.PosZDouble
import org.typelevel.discipline.scalatest.Discipline

class BigRationalLawsTest extends FunSuite with Discipline {

  // we discard zero denominators, so we need a higher maxiscardedFactor than the default 5.0:
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(maxDiscardedFactor = PosZDouble(50.0))

  implicit def isArbitrary: Arbitrary[BigRational] = Arbitrary(BigRationalTest.rational)

  implicit val isAdditiveMonoid: AdditiveMonoid[BigRational] = new AdditiveMonoid[BigRational] {
    override def zero: BigRational = BigRational.zero
    override def plus(x: BigRational, y: BigRational): BigRational = x + y
  }

  implicit val isField: Field[BigRational] = new Field[BigRational] {
    override def zero: BigRational = BigRational.zero
    override def plus(x: BigRational, y: BigRational): BigRational = x + y
    override def negate(x: BigRational): BigRational = - x
    override def one: BigRational = BigRational.one
    override def times(x: BigRational, y: BigRational): BigRational = x * y
    override def div(x: BigRational, y: BigRational): BigRational = x / y
  }

  implicit def isEq: Eq[BigRational] = Eq.fromUniversalEquals

  checkAll("BigRational.CommutativeRingLaws", RingLaws[BigRational].field)
}
