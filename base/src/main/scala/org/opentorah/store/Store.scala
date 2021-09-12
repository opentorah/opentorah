package org.opentorah.store

import org.opentorah.metadata.{Named, Names}

trait Store extends Named

object Store:
  type Path = Seq[Store]

  trait Terminal extends Store

  trait NonTerminal extends Store, Stores

  trait Numbered extends Store:
    final override def names: Names = Names(number.toString)

    def number: Int
