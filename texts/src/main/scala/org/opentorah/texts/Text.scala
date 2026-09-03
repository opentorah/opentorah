package org.opentorah.texts

import org.podval.metadata.Names
import org.podval.store.{Store, Stores}

// TODO expose Rambam here too!
object Text extends Stores[?]:
  override val names: Names = Names("Jewish Texts")
  override def stores: Seq[Store] = Seq(tanach.Tanach)
