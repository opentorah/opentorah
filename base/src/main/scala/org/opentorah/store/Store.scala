package org.opentorah.store

import org.opentorah.metadata.{Named, Names}

sealed trait Store extends Named

object Store:
  type Path = Seq[Store]

  trait Terminal extends Store

  trait NonTerminal[+T <: Store] extends Store, Stores[T]

  trait Pure[+T <: Store] extends NonTerminal[T], Stores.Pure[T]

  trait Bys extends Pure[By[?]]

  trait Numbered extends Store, org.opentorah.metadata.Numbered[Numbered]:
    def oneOf: Stores.Numbered[Numbered]
    final override def names: Names = oneOf.number2names(number)
