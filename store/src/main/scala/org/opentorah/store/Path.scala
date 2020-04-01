package org.opentorah.store

final case class Path(path: Seq[Binding]) {

  def isEmpty: Boolean = path.isEmpty

  def length: Int = path.length

  def ++(that: Path): Path = copy(path = path ++ that.path)

  def :+(binding: Binding): Path = copy(path = path :+ binding)

  def init: Path = copy(path = path.init)

  def last: Binding = path.last

  def tail: Path = copy(path = path.tail)
}

object Path {
  val empty: Path = new Path(Seq.empty)
}
