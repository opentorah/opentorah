package org.opentorah.metadata

import org.opentorah.util.Util

trait Named extends WithName with WithNames {
  override def name: String = Util.className(this)

  override def toString: String = name
}
