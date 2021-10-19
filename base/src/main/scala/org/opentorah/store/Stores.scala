package org.opentorah.store

import org.opentorah.metadata.HasValues
import org.opentorah.util.{Effects, Files}
import org.opentorah.xml.Caching
import zio.ZIO

// TODO move store package into site project?
trait Stores[+T <: Store] extends Store:
  def stores: Caching.Parser[Seq[T]]

  def findByName(name: String): Caching.Parser[Option[T]] = stores.map(HasValues.find(_, name))

  // TODO add indexOf() and friends

  /*
  Successful `resolve()` returns a 'path' - a sequence of `Stores`;
  URL resolved by such a path can be reconstructed from it (in its canonical form);
  such a reconstructed URL should resolve to the same path (TODO add tests for this ;)).

  Not all `Stores` are read from XML - some are constructed -
  so `Store` does *not* extend `FromUrl.With`.

  Store.Path returned is nonEmpty ;)
 */
  final def resolve(path: String): Caching.Parser[Path] = resolve(Files.splitAndDecodeUrl(path))

  // TODO does this work with an alias "/" - and should such alias be legal?
  final def resolve(path: Seq[String]): Caching.Parser[Path] =
    if path.nonEmpty then this.resolve(path, Seq.empty) else ZIO.succeed(Seq(this))

  private def resolve(
    path: Seq[String],
    acc: Path
  ): Caching.Parser[Path] = if path.isEmpty then ZIO.succeed(acc.reverse) else
    val head: String = path.head
    val tail: Seq[String] = path.tail
    for
      nextOpt: Option[Store] <- findByName(head)
      _ <- Effects.check(nextOpt.nonEmpty, s"Did not find '$head' in $this")
      result <- nextOpt.get match {
        case alias: Alias =>
          for
            toPath: Path <- resolve(alias.to)
            stores: Stores[?] = toPath.last.asInstanceOf[Stores[?]]
            result <- stores.resolve(tail, toPath.reverse ++ acc)
          yield result
        case stores: Stores[?] => stores.resolve(tail, stores +: acc)
        case next if tail.isEmpty => ZIO.succeed((next +: acc).reverse)
        case next => Effects.fail(s"Can not apply '$tail' to $next")
      }
    yield result
