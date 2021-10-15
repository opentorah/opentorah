package org.opentorah.texts.tanach

import org.opentorah.store.Store

abstract class Verse(override val number: Int) extends Store.Numbered, Store.Terminal
