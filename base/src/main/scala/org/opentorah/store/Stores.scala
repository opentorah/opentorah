package org.opentorah.store

import org.opentorah.util.{Effects, Strings}
import org.opentorah.xml.Parser
import zio.ZIO

// TODO
// - RootStore as a base for Site;
// - StoreAlias;
// - move store package into site project;
// - push Hierarchy etc. into site by *not* keeping the path maps but calculating them on the fly;
trait Stores[+T <: Store]:
  def findByName(name: String): Caching.Parser[Option[T]] =
    stores.map(_.find(_.names.hasName(name)))

  def stores: Caching.Parser[Seq[T]]

  // TODO add indexOf(), Comparable...

object Stores:

  // TODO maybe pre-calculate a lazy map from all names to stores?
  trait Pure[+T <: Store] extends Stores[T]:
    final override def stores: Caching.Parser[Seq[T]] = ZIO.succeed(storesPure)
    
    protected def storesPure: Seq[T]

    private def get(name: String, result: Option[T]): T =
      require(result.isDefined, s"Unknown $this: $name")
      result.get

    final def indexOf(store: Store): Int = storesPure.indexWhere(store eq _, 0)

    final def distance(from: Store, to: Store): Int = indexOf(to) - indexOf(from)

    //final val ordering: Ordering[Key] = (x: Key, y: Key) => distance(x, y)
  
  // TODO override indexOf()
  trait Numbered[+T <: Store.Numbered] extends Pure[T]:
    final override def findByName(name: String): Parser[Option[T]] = ZIO.succeed(
      name.toIntOption.flatMap(number =>
        if number < minNumber || number > maxNumber then None
        else Some(createNumberedStore(number))
      )
    )

    override def storesPure: Seq[T] =
      minNumber.to(maxNumber).map(createNumberedStore)

    final  def maxNumber: Int = minNumber + length - 1

    def minNumber: Int = 1

    def length: Int

    protected def createNumberedStore(number: Int): T

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
    current: Stores[Store]
  ): Caching.Parser[Store.Path] =
    // TODO handle empty paths smoother: include starting Store?
    require(path.nonEmpty)
    Stores.resolve(path, current, Seq.empty)

  private def resolve(
    path: Seq[String],
    current: Stores[Store],
    acc: Store.Path
  ): Caching.Parser[Store.Path] =
    if path.isEmpty then ZIO.succeed(acc.reverse) else
      val head: String = path.head
      val tail: Seq[String] = path.tail
      current.findByName(head).flatMap { _.fold(Effects.fail(s"Did not find '$head' in $current")) {
        case next: Stores[Store] => resolve(tail, next, next +: acc)
        case next if tail.isEmpty => ZIO.succeed((next +: acc).reverse)
        case next => Effects.fail(s"Can not apply '$tail' to $next")
      }}
