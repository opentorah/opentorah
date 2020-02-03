package org.digitaljudaica.store.metadata

import org.digitaljudaica.store.util.Util

trait Named extends WithName with WithNames {
  override def name: String = Util.className(this)

  override def toString: String = name
}
