package org.opentorah.metadata

trait Numbered[T] {

  def number: Int

  final override def equals(other: Any): Boolean = other match {
    case that: Numbered[T] => number == that.number
    case _ => false
  }

  final override def hashCode: Int = number

  override def toString: String = number.toString
}

object Numbered {
  import scala.language.implicitConversions

  implicit def numberedOrdering[T <: Numbered[T]]: Ordering[T] = (x: T, y: T ) => x.number - y.number

  implicit def numberedOrderingOps[T <: Numbered[T]](lhs: T): Ordering[T]#OrderingOps = numberedOrdering.mkOrderingOps(lhs)
}
