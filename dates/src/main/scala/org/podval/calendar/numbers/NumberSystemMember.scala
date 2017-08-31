package org.podval.calendar.numbers

trait NumberSystemMember[S <: NumberSystem[S]] {
  def numberSystem: S
}
