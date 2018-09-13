package org.podval.calendar.metadata

abstract class Numbered[T <: Numbered[T]](val number: Int) extends Ordered[T] {

  final override def equals(other: Any): Boolean = other match {
    case that: Numbered[_] => number == that.number
    case _ => false
  }

  final override def hashCode: Int = number

  final override def compare(that: T): Int = this.number - that.number

  override def toString: String = number.toString
}
