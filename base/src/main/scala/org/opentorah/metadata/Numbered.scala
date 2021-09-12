package org.opentorah.metadata

trait Numbered[T] derives CanEqual:
  def number: Int

  final override def equals(other: Any): Boolean = number == other.asInstanceOf[Numbered[T]].number

  final override def hashCode: Int = number

  override def toString: String = number.toString

object Numbered:
  given numberedOrdering[T <: Numbered[T]]: Ordering[T] = (x: T, y: T ) => x.number - y.number

  import scala.language.implicitConversions
  implicit def numberedOrderingOps[T <: Numbered[T]](lhs: T): Ordering[T]#OrderingOps = numberedOrdering.mkOrderingOps(lhs)
