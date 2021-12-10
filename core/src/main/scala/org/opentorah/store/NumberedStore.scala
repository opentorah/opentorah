package org.opentorah.store

import org.opentorah.metadata.Names

trait NumberedStore extends Store, org.opentorah.metadata.Numbered[NumberedStore]:
  def oneOf: NumberedStores[NumberedStore]

  final override def names: Names = oneOf.number2names(number)