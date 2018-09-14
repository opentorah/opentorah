package org.podval.calendar.generate.tanach

object CustomParser {

  def parse(names: String): Set[Custom] = {
    val customs: Seq[Custom] = names.split(' ').map(_.trim).map(resolve)
    require(customs.length == customs.toSet.size, s"Duplicate customs: $customs")
    val result = customs.toSet

    // TODO add customs that have all children already in the set - recursively.

    // Remove customs that have parents in the set.
    result.filterNot(custom => custom.parent.isDefined && result.contains(custom.parent.get))
  }

  def resolve(name: String): Custom = {
    val result = Custom.forName(name)
    require(result.isDefined, s"Unknown custom: $name")
    result.get
  }

  def checkDisjoint(sets: Set[Set[Custom]]): Unit = {
    // TODO
  }
}
