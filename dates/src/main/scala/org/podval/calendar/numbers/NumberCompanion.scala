package org.podval.calendar.numbers

trait NumberCompanion[S <: NumberSystem[S], N <: Number[S, N]] extends NumberSystemMember[S] {
  val zero: N = apply(0)

  def apply(digits: Int*): N

  final def fromDigits(digits: Seq[Int]): N = apply(digits: _*)

  final def fromRational(value: BigRational, length: Int = numberSystem.defaultLength): N = {
    val digits: Seq[Int] = numberSystem.from[BigRational](
      signum = value.signum,
      value = value.abs,
      length = length,
      wholeAndFraction = _.wholeAndFraction,
      mult = _ * _,
      round = BigRational.round
    )

    fromDigits(digits)
  }

  final def fromDouble(value: Double, length: Int = numberSystem.defaultLength): N = {
    def wholeAndFraction(what: Double): (Int, Double) = {
      val whole: Double = math.floor(what)
      (whole.toInt, what - whole)
    }

    def round(whole: Int, fraction: Double): Int = whole + math.round(fraction).toInt

    val digits = numberSystem.from[Double](
      signum = math.signum(value).toInt,
      value = math.abs(value),
      length = length,
      wholeAndFraction = wholeAndFraction,
      mult = _ * _,
      round = round
    )

    fromDigits(digits)
  }
}
