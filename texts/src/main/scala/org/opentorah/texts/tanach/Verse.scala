package org.opentorah.texts.tanach

import org.podval.store.{NumberedStore, NumberedStores}

final class Verse(
  override val number: Int,
  override val oneOf: NumberedStores[Verse]
) extends NumberedStore
