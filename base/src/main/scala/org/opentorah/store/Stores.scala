package org.opentorah.store

import org.opentorah.util.{Effects, Strings}
import org.opentorah.xml.Parser
import zio.ZIO

trait Stores {
  def findByName(name: String): Caching.Parser[Option[Store]] =
    stores.map(_.find(_.names.hasName(name)))

  def stores: Caching.Parser[Seq[Store]]

  // TODO add indexOf(), Comparable...
}

object Stores {

  trait Pure extends Stores {
    final override def stores: Caching.Parser[Seq[Store]] = ZIO.succeed(storesPure)

    protected def storesPure: Seq[Store]
  }

  trait Numbered[T <: Store.Numbered] extends Pure {
    final override def findByName(name: String): Parser[Option[T]] = ZIO.succeed {
      Strings.toInt(name).flatMap { number =>
        if (number < minNumber || number > maxNumber) None
        else Some(createNumberedStore(number))
      }
    }

    override def storesPure: Seq[Store] =
      minNumber.to(maxNumber).map(createNumberedStore)

    final  def maxNumber: Int = minNumber + length - 1

    def minNumber: Int = 1

    def length: Int

    protected def createNumberedStore(number: Int): T
  }

  /*
    Successful `resolve()` returns a 'path' - a sequence of `Stores`;
    URL resolved by such a path can be reconstructed from it (in its canonical form);
    such a reconstructed URL should resolve to the same path (TODO add tests for this ;)).

    Not all `Stores` are read from XML - some are constructed -
    so `Store` does *not* extend `FromUrl.With`.

    Store.Path returned is nonEmpty ;)
   */
  // TODO I should probably try using ZIO error channel instead of Either,
  // but for that a lot of types need to change, and the first of them - Effects.Error should be
  // derived from Throwable (and then I can type all lines in the 'for's :))
  def resolve(
    path: Seq[String],
    current: Stores
  ): Caching.Parser[Store.Path] = {
    // TODO handle empty paths smoother: include starting Store?
    require(path.nonEmpty)
    Stores.resolve(path, current, Seq.empty)
  }

  private def resolve(
    path: Seq[String],
    current: Stores,
    acc: Store.Path
  ): Caching.Parser[Store.Path] =
    if (path.isEmpty) ZIO.succeed(acc.reverse) else {
      val head: String = path.head
      val tail: Seq[String] = path.tail
      current.findByName(head).flatMap {
        case Some(next: Stores) => resolve(tail, next, next +: acc)
        case Some(next) if tail.isEmpty => ZIO.succeed((next +: acc).reverse)
        case Some(next) => Effects.fail(s"Can not apply '$tail' to $next")
        case None       => Effects.fail(s"Did not find '$head' in $current")
      }
    }
}
