package org.opentorah.metadata

import org.opentorah.util.Util
import org.opentorah.xml.Parser

trait NamedCompanion {
  type Key <: Named

  def values: Seq[Key]

  // This is:
  // - lazy to allow correct initialization: the code uses values(),
  //   Language metadata file references Language instances by name :)
  // - public so that it can be accessed from the Key type if it isn't defined
  //   within the object derived from NamedCompanion;
  // - not final so that it can be overridden in Tanach, for instance.
  lazy val toNames: Map[Key, Names] = Parser.parseDo(Metadata.loadNames(
    obj = this,
    resourceName,
    keys = values
  ))

  protected def resourceName: String = what

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

  final def numberOfValues: Int = values.length

  final def forIndex(index: Int): Key = values(index)

  final def indexOf(key: Key): Int = values.indexOf(key)

  final def next(key: Key): Key = forIndex(indexOf(key) + 1)

  final def distance(from: Key, to: Key): Int = indexOf(to) - indexOf(from)

  final val ordering: Ordering[Key] = (x: Key, y: Key) => distance(x, y)

  private def what: String = Util.className(this)
}
