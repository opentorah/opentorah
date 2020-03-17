package org.opentorah.archive.collector.selectors

import org.opentorah.metadata.{LanguageSpec, Name, Names}
import org.opentorah.store.Selector

case object DocumentSelector extends Selector.Named {
  override val names: Names = new Names(Seq(new Name("документ", LanguageSpec.empty)))
}
