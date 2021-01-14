package org.opentorah.numbers

/**
  * Operations needed to find a zero crossing of a function.
  *
  * @tparam T  type of the number to convert to/from
  */
trait Zeroable[T] {
  def signum(value: T): Int
}

object Zeroable {
  def apply[T](implicit ev: Zeroable[T]): Zeroable[T] = ev

  implicit class ZeroableOps[T: Zeroable](value: T) {
    def signum: Int = Zeroable[T].signum(value)
  }

  implicit def numberZeroable[N <: Numbers#Number[N]]: Zeroable[N] = new Zeroable[N] {
    override def signum(value: N): Int = value.signum
  }
}
