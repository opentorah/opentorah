package org.opentorah.store

import org.opentorah.metadata.LanguageSpec

final case class Path(path: Seq[Binding]) {

  def ++(that: Path): Path = copy(path = path ++ that.path)

  def :+(binding: Binding): Path = copy(path = path :+ binding)

  def init: Path = copy(path = path.init)

  def last: Binding = path.last

  // TODO rename?
  def reference(languageSpec: LanguageSpec): String = path.last.selectedName(languageSpec)
}

object Path {
  val empty: Path = new Path(Seq.empty)
}
