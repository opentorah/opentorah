package org.opentorah.numbers

import cats.Eq
import cats.kernel.CommutativeGroup
import cats.kernel.laws.discipline.CommutativeGroupTests
import org.podval.xml.XmlParser
import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

abstract class NumbersLawsTest(val numbers: Numbers) extends AnyFunSuite, FunSuiteDiscipline, Checkers:
  import numbers.given

  // Modest heads: NonPeriodic used to pick `0 until Int.MaxValue`, which overflows `Int` digits on `+`/`*`.
  private val headDigit: Gen[Int] = Gen.choose(-200, 200)

  // At most 3 tail digits so Vector/Vector quotients fit in `Int` (`BigRational.whole`).
  private val tailDigits: Gen[Seq[Int]] =
    val length: Int = math.min(3, numbers.maxLength)
    Gen.sequence[Seq[Int], Int](
      numbers.ranges.take(length).map((range: Int) => Gen.choose(-2 * range, 2 * range))
    )

  def digits: Gen[Seq[Int]] = for
    head <- headDigit
    tail <- tailDigits
  yield head +: tail

  def vectors: Gen[numbers.Vector] = digits.map(d => numbers.Vector.fromDigits(d))

  def nonZeroVectors: Gen[numbers.Vector] = vectors.suchThat(!_.isZero)

  def points: Gen[numbers.Point] = digits.map(d => numbers.Point.fromDigits(d))

  private val smallInt: Gen[Int] = Gen.choose(-20, 20)

  private val nonZeroSmallInt: Gen[Int] = smallInt.suchThat(_ != 0)

  given isArbitrary: Arbitrary[numbers.Vector] = Arbitrary(vectors)

  given isEq: Eq[numbers.Vector] = Eq.fromUniversalEquals

  given CommutativeGroup[numbers.Vector] with
    override def empty: numbers.Vector = numbers.Vector.zero
    override def combine(x: numbers.Vector, y: numbers.Vector): numbers.Vector = x + y
    override def inverse(a: numbers.Vector): numbers.Vector = -a

  checkAll(XmlParser.className(numbers.getClass), CommutativeGroupTests[numbers.Vector].commutativeGroup)

  test("Int acts as a module on Vector"):
    check(Prop.forAll(vectors, vectors, smallInt) { (x, y, n) =>
      (x + y) * n == x * n + y * n
    })
    check(Prop.forAll(vectors, smallInt, smallInt) { (x, m, n) =>
      x * (m + n) == x * m + x * n
    })
    check(Prop.forAll(vectors, smallInt, smallInt) { (x, m, n) =>
      (x * m) * n == x * (m * n)
    })
    check(Prop.forAll(vectors) { x =>
      x * 1 == x && x * 0 == numbers.Vector.zero
    })
    check(Prop.forAll(vectors, smallInt) { (x, n) =>
      -(x * n) == (-x) * n && (-x) * n == x * (-n)
    })

  test("Euclidean identity for Vector / Vector"):
    check(Prop.forAll(vectors, nonZeroVectors) { (x, y) =>
      x == y * (x / y) + (x % y)
    })

  test("Euclidean identity for Vector / Int"):
    check(Prop.forAll(vectors, nonZeroSmallInt) { (x, n) =>
      val length: Int = numbers.maxLength
      x == (x / (n, length)) * n + (x % (n, length))
    })

  test("Point is an affine space over Vector"):
    check(Prop.forAll(points) { p =>
      (p + numbers.Vector.zero) == p && (p - p) == numbers.Vector.zero
    })
    check(Prop.forAll(points, points) { (p, q) =>
      (p - q) + q == p
    })

  numbers.headRangeOpt.foreach: (headRange: Int) =>
    val period: numbers.Vector = numbers.Vector(headRange)

    test("periodicity"):
      check(Prop.forAll(points) { p =>
        (p + period) == p
      })
      check(Prop.forAll(vectors) { v =>
        (v + period).canonical == v.canonical
      })
      check(Prop.forAll(vectors) { v =>
        val canonicalHead: Int = v.canonical.get(0)
        canonicalHead >= 0 && canonicalHead < headRange
      })
