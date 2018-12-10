package org.podval.judaica.metadata

import org.podval.judaica.util.Util

trait NamedCompanion {
  type Key <: Named

  def values: Seq[Key]

  // This is public so that it can be accessed from the Key type if it isn't defined within the object derived from Named.
  // This isn't final so that it can be overriden in Tanach, for instance.
  lazy val toNames: Map[Key, Names] = Metadata.loadNames(values, this, resourceName)

  protected def resourceName: String = Util.className(this)

  final def forDefaultName(name: String): Option[Key] = values.find(_.name == name)

  final def getForDefaultName(name: String): Key = {
    val result = forDefaultName(name)
    require(result.isDefined, s"Unknown $what: $name")
    result.get
  }

  final def forName(name: String): Option[Key] = values.find(_.names.hasName(name))

  final def getForName(name: String): Key = {
    val result = forName(name)
    require(result.isDefined, s"Unknown $what: $name")
    result.get
  }

  final def forIndex(index: Int): Key = values(index)

  final def indexOf(key: Key): Int = values.indexOf(key)

  final def next(key: Key): Key = forIndex(indexOf(key) + 1)

  final def distance(from: Key, to: Key): Int = indexOf(to) - indexOf(from)

  final val ordering: Ordering[Key] = new Ordering[Key] {
    final override def compare(x: Key, y: Key): Int = distance(x, y)
  }

  private def what: String = Util.className(this)
}
