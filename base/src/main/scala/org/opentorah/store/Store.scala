package org.opentorah.store

import org.opentorah.metadata.{Language, Named}
import org.opentorah.xml.Caching
import zio.ZIO

trait Store extends Named:

  final def getPaths(
    path: Path = Seq.empty,
    include: Store => Boolean,
    stop: Store => Boolean
  ): Caching.Parser[Seq[Path]] =
    val selfPath: Path = path :+ this
    val self: Seq[Path] = if include(this) then Seq(selfPath) else Seq.empty
    this match
      case stores: Stores[?] if !stop(this) =>
        for
          stores: Seq[Store] <- stores.stores
          results: Seq[Seq[Path]] <- ZIO.foreach(stores)(_.getPaths(selfPath, include, stop))
        yield self ++ results.flatten
      case _ => ZIO.succeed(self)
