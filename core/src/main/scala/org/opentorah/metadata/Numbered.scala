package org.opentorah.metadata

// TODO move into util?
trait Numbered[T] extends Ordered[Numbered[T]]:
  def number: Int

  override def compare(that: Numbered[T]): Int = this.number - that.number

  final override def equals(other: Any): Boolean = other match
    case that: Numbered[?] => this.number == that.number
    case _ => false

  final override def hashCode: Int = number

  override def toString: String = number.toString
