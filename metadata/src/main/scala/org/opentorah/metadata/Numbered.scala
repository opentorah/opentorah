package org.opentorah.metadata

trait Numbered {

  def number: Int

  type T

  final override def equals(other: Any): Boolean = other match {
    case that: Numbered => number == that.number
    case _ => false
  }

  final override def hashCode: Int = number

  override def toString: String = number.toString
}

object Numbered {
  import scala.language.implicitConversions

  implicit def numberedOrdering[T <: Numbered]: Ordering[T] = (x: T, y: T ) => x.number - y.number

  implicit def numberedOrderingOps[T <: Numbered](lhs: T): Ordering[T]#Ops = numberedOrdering.mkOrderingOps(lhs)
}
