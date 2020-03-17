package org.opentorah.store

import org.opentorah.metadata.Named

trait Selector extends Named

object Selector {

  trait Numbered extends Selector

  trait Named extends Selector

  trait Nullary extends Selector
}
