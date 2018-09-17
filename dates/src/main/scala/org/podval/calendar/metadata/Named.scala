package org.podval.calendar.metadata

trait Named {
  type Key <: Named.NamedBase

  val values: Seq[Key]

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

  final def distance(from: Key, to: Key): Int = indexOf(to) - indexOf(from)

  def what: String = Named.className(this)
}

object Named {
  trait HasNames {
    def names: Names

    final def hasName(name: String): Boolean = names.hasName(name)

    // TODO toString = names.doFind(LanguageSpec.empty).name
  }

  trait NamedBase extends HasNames {
    def name: String = Named.className(this)

    override def toString: String = name

    final def toString(spec: LanguageSpec): String = names.doFind(spec).name
  }

  // TODO this breaks on inner classes; fixed in JDK 9...
  def className(obj: AnyRef): String = obj.getClass.getSimpleName.replace("$", "")
}
