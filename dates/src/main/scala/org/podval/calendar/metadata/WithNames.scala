package org.podval.calendar.metadata

trait WithNames[K <: WithNames[K]] extends Named { this: K =>
  final override def names: Names = toNames(this)

  def toNames: Map[K, Names]
}
