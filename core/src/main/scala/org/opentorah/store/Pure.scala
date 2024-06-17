package org.opentorah.store

import org.opentorah.xml.Parser
import zio.ZIO

// TODO maybe pre-calculate a lazy map from all names to stores?
trait Pure[+T <: Store] extends Stores[T]:
  final override def stores: Parser[Seq[T]] = ZIO.succeed(storesPure)

  protected def storesPure: Seq[T]

  private def get(name: String, result: Option[T]): T =
    require(result.isDefined, s"Unknown $this: $name")
    result.get

object Pure:
  trait With[+T <: Store](override val storesPure: Seq[T]) extends Pure[T]
