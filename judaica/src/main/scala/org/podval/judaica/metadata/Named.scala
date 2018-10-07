package org.podval.judaica.metadata

trait Named {
  type Key <: Named.NamedBase

  val values: Seq[Key]

  // This is public so that it can be accessed from the Key type if it isn't defined within the object derived from Named.
  // This isn't final so that it can be overriden in Tanach, for instance.
  lazy val toNames: Map[Key, Names] = Metadata.loadNames(values, this, resourceName)

  protected def resourceName: String = Named.className(this)

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

  final def what: String = Named.className(this)
}

object Named {
  trait HasName {
    def hasName(name: String): Boolean
  }

  trait HasNames extends HasName {
    def names: Names

    final override def hasName(name: String): Boolean = names.hasName(name)

    // TODO toString = names.doFind(LanguageSpec.empty).name
  }

  // TODO Collapse HasName/HasNames/NamedBase -> WithNames; make remaining global?
  trait NamedBase extends HasNames {
    def name: String = Named.className(this)

    override def toString: String = name

    final def toString(spec: LanguageSpec): String = names.doFind(spec).name
  }

  // TODO this breaks on inner classes; fixed in JDK 9...
  def className(obj: AnyRef): String = obj.getClass.getSimpleName.replace("$", "")
}
