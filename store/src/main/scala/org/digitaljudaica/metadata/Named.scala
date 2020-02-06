package org.digitaljudaica.metadata

import org.digitaljudaica.util.Util

trait Named extends WithName with WithNames {
  override def name: String = Util.className(this)

  override def toString: String = name
}
