package org.opentorah.texts

import org.opentorah.store.{Store, Stores}

// TODO expose Rambam here too!
object Text extends Stores.Pure[Store]:
  override def storesPure: Seq[Store] = Seq(tanach.Tanach)
