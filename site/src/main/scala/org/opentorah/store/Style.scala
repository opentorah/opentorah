package org.opentorah.store

object Style:

  trait Wide extends Store:
    final override def style: String = "wide"

  val default: String = "main"
