package org.opentorah.store

import org.opentorah.metadata.{Named, Names}

trait Store extends Named

object Store:
  type Path = Seq[Store]

  trait Terminal extends Store

  trait NonTerminal[+T <: Store] extends Store, Stores[T]

  // TODO use metadata.Numbered
  trait Numbered extends Store:
    // TODO Hebrew!
    final override def names: Names = Names(number.toString)

    def number: Int
