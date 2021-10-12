package org.opentorah.store

import org.opentorah.metadata.{HasValues, Language, Name, Names}
import org.opentorah.util.{Effects, Strings}
import org.opentorah.xml.Parser
import zio.ZIO

// TODO
// - RootStore as a base for Site?
// - StoreAlias;
// - move store package into site project?
// - push Hierarchy etc. into site by *not* keeping the path maps but calculating them on the fly;
trait Stores[+T <: Store]:
  def findByName(name: String): Caching.Parser[Option[T]] = stores.map(HasValues.find(_, name))
  def stores: Caching.Parser[Seq[T]]
  // TODO add indexOf() and friends

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
    def minNumber: Int = 1
    // Derived types must override one of the:
    def maxNumber: Int = minNumber + length - 1
    def length: Int = maxNumber - minNumber + 1

    final def contains(number: Int): Boolean = minNumber <= number && number <= maxNumber

    def name2number(name: String): Option[Int] =
      name.toIntOption.orElse(Language.Hebrew.numberFromString(name))

    def number2names(number: Int): Names = new Names(Seq(
      Name(number.toString, Language.Spec.empty),
      Name(Language.Hebrew.numberToString(number), Language.Hebrew.toSpec)
    ))

    override def storesPure: Seq[T] = minNumber.to(maxNumber).map(createNumberedStore)
    protected def createNumberedStore(number: Int): T

    final override def findByName(name: String): Parser[Option[T]] =
      ZIO.succeed(name2number(name).filter(contains).map(createNumberedStore))

  /*
    Successful `resolve()` returns a 'path' - a sequence of `Stores`;
    URL resolved by such a path can be reconstructed from it (in its canonical form);
    such a reconstructed URL should resolve to the same path (TODO add tests for this ;)).

    Not all `Stores` are read from XML - some are constructed -
    so `Store` does *not* extend `FromUrl.With`.

    Store.Path returned is nonEmpty ;)
   */
  def resolve(
    path: Seq[String],
    current: Stores[Store]
  ): Caching.Parser[Store.Path] = if path.nonEmpty then Stores.resolve(path, current, Seq.empty) else
    if current.isInstanceOf[Store] then ZIO.succeed(Seq(current.asInstanceOf[Store]))
    else Effects.fail(s"Path is empty at $current")

  private def resolve(
    path: Seq[String],
    current: Stores[Store],
    acc: Store.Path
  ): Caching.Parser[Store.Path] = if path.isEmpty then ZIO.succeed(acc.reverse) else
    val head: String = path.head
    val tail: Seq[String] = path.tail
    current.findByName(head).flatMap { _.fold(Effects.fail(s"Did not find '$head' in $current")) {
      case next: Stores[Store] => resolve(tail, next, next +: acc)
      case next if tail.isEmpty => ZIO.succeed((next +: acc).reverse)
      case next => Effects.fail(s"Can not apply '$tail' to $next")
    }}
