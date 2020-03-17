package org.opentorah.archive.collector.selectors

import org.opentorah.metadata.{LanguageSpec, Name, Names}
import org.opentorah.store.Selector

case object NameSelector extends Selector.Named {
  override val names: Names = new Names(Seq(new Name("имя", LanguageSpec.empty)))
}
