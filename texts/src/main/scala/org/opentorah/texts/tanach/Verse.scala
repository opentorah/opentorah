package org.opentorah.texts.tanach

import org.opentorah.store.{NumberedStore, Store, Terminal}

abstract class Verse(override val number: Int) extends NumberedStore, Terminal
