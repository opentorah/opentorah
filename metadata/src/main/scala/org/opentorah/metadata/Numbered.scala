package org.opentorah.metadata

trait Numbered[T <: Numbered[T]] extends Ordered[T] {

  def number: Int

  final override def equals(other: Any): Boolean = other match {
    case that: Numbered[_] => number == that.number
    case _ => false
  }

  final override def hashCode: Int = number

  final override def compare(that: T): Int = this.number - that.number

  override def toString: String = number.toString
}
