package org.podval.calendar.metadata

trait Named {
  type Key <: Names.NamedBase

  def bind[M <: Names.HasNames](keys: Seq[Key], metadatas: Seq[M]): Seq[(Key, M)] = {
    require(keys.length == metadatas.length)
    keys.zip(metadatas).map { case (key, metadata) =>
      require(metadata.names.has(key.name))
      key -> metadata
    }
  }

  val values: Seq[Key]

  final def forDefaultName(name: String): Option[Key] = values.find(_.name == name)

  final def getForDefaultName(name: String): Key = {
    val result = forDefaultName(name)
    require(result.isDefined, s"Unknown $what: $name")
    result.get
  }

  final def forName(name: String): Option[Key] = values.find(_.names.has(name))

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
  // TODO this breaks on inner classes; fixed in JDK 9...
  def className(obj: AnyRef): String = obj.getClass.getSimpleName.replace("$", "")
}
