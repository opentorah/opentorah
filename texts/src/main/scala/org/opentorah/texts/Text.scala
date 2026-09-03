package org.opentorah.texts

import org.podval.metadata.Names
import org.podval.store.{Pure, Store}

// TODO expose Rambam here too!
object Text extends Pure[?]:
  override val names: Names = Names("Jewish Texts")
  override def storesPure: Seq[Store] = Seq(tanach.Tanach)
