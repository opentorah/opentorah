package org.podval.calendar.metadata

trait WithNamesCompanion[K <: WithNames[K]] {
  val values: Seq[K]

  def resourceName: String = Named.className(this)

  // This is lazy to allow Language to initialize correctly: its metadata file references Language instances by name :)
  final lazy val toNames: Map[K, Names] = MetadataParser.loadNames(this, resourceName, values)

  final def forCode(name: String): Option[K] = values.find(_.name == name)

  final def forName(name: String): Option[K] = values.find(_.names.has(name))

  // TODO non-Option-returning flavours that throw exception?
}
