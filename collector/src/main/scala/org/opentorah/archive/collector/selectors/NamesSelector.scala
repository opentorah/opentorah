package org.opentorah.archive.collector.selectors

import org.opentorah.metadata.{LanguageSpec, Name, Names}
import org.opentorah.store.Selector

case object NamesSelector extends Selector.Nullary {
  // TODO name of this is no longer affected by what is in the names-lists.xml;
  // when Store comes into play and Selectors are read from there,
  // this will get fixed :)
  override val names: Names = new Names(Seq(new Name("Имена", LanguageSpec.empty)))
}
