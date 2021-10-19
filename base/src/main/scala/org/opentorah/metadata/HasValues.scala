package org.opentorah.metadata

// TODO after total ZIOfication this will get blended into Store, required etc.
trait HasValues[+T]:
  def valuesSeq: Seq[T] // TODO switch to values: Array[Key] and eliminate

  final def numberOfValues: Int = valuesSeq.length

object HasValues:

  trait Distance[T <: AnyRef] extends HasValues[T]:
    final def indexOf(value: T): Int = valuesSeq.indexWhere(value eq _, 0)

    final def distance(from: T, to: T): Int = indexOf(to) - indexOf(from)

    //final val ordering: Ordering[Key] = (x: Key, y: Key) => distance(x, y)

  trait FindByDefaultName[+T <: HasName] extends HasValues[T]:
    final def getForDefaultName(name: String): T = get(name, forDefaultName(name), this)
    final def forDefaultName(name: String): Option[T] = valuesSeq.find(_.name == name)

  trait FindByName[+T <: Named] extends HasValues[T]:
    final def getForName(name: String): T = get(name, forName(name), this)
    final def forName(name: String): Option[T] = find(valuesSeq, name)

  private def get[T](name: String, result: Option[T], where: AnyRef): T =
    require(result.isDefined, s"Unknown $where: $name")
    result.get

  def find[T <: Named](valuesSeq: Seq[T], name: String): Option[T] = valuesSeq.find(_.names.hasName(name))