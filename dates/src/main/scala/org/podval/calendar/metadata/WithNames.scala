package org.podval.calendar.metadata

trait WithNames[K <: WithNames[K]] extends Named { this: K =>
  final override def names: Names = companion.toNames(this)

  def companion: WithNamesCompanion[K]
}
