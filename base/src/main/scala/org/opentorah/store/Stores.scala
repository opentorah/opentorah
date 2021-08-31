package org.opentorah.store

import org.opentorah.util.Files
import org.opentorah.xml.Parser
import zio.ZIO

trait Stores {
  // TODO add getBys and use it in resolve()!

  def findByName(name: String): Caching.Parser[Option[Store]]
}

object Stores {

  trait NonTerminal extends Stores {

    override def findByName(name: String): Caching.Parser[Option[Store]] =
      findByNameAmongNonTerminalStores(name)

    protected final def findByNameAmongNonTerminalStores(name: String): Caching.Parser[Option[Store.NonTerminal]] =
      findByNameAmongStores(name, nonTerminalStores)

    protected def nonTerminalStores: Seq[Store.NonTerminal]
  }

  trait Terminal extends Stores {

    override def findByName(name: String): Caching.Parser[Option[Store]] =
      findByNameAmongTerminalStores(name)

    protected final def findByNameAmongTerminalStores(name: String): Caching.Parser[Option[Store.Terminal]] =
      findByNameWithExtension(
        name = name,
        findByName = name => findByNameAmongStores(name, terminalStores)
      )

    protected def terminalStores: Seq[Store.Terminal]
  }

  /*
    Successful `resolve()` returns a 'path' - a sequence of `Stores`;
    URL resolved by such a path can be reconstructed from it (in its canonical form);
    such a reconstructed URL should resolve to the same path (TODO add tests for this ;)).

    Not all `Stores` are read from XML - some are constructed -
    so `Store` does *not* extend `FromUrl.With`.
   */
  def resolve(
    path: Seq[String],
    current: Stores,
    acc: Store.Path
  ): Caching.Parser[Option[Store.Path]] =
    if (Stores.isEndOfPath(path)) Stores.endOfPath(acc) else current.findByName(path.head) flatMap {
      case Some(next: Stores) => resolve(path.tail, next, next +: acc)
      case Some(next) if Stores.isEndOfPath(path.tail) => Stores.endOfPath(next +: acc)
      case _ => ZIO.succeed(None) // Note: with the None pattern, I get "match may not be exhaustive" warning...
    }

  private def findByNameAmongStores[T <: Store](
    name: String,
    stores: Seq[T]
  ): Parser[Option[T]] =
    ZIO.succeed(stores.find(_.names.hasName(name)))

  def findByNameWithExtension[M <: Store](
    name: String,
    findByName: String => Caching.Parser[Option[M]],
    allowedExtension: String = "html",
    assumeAllowedExtension: Boolean = false
  ): Caching.Parser[Option[M]] = {
    val (fileName: String, extension: Option[String]) = Files.nameAndExtension(name)

    val result: Option[String] =
      if (extension.isDefined && !extension.contains(allowedExtension))
        if (assumeAllowedExtension) Some(name) else None
      else
        Some(fileName)

    result.fold[Caching.Parser[Option[M]]](ZIO.none)(findByName)
  }

  // TODO this is too HTML-ly for the 'store' package and should be something like isDirectory/isTerminal
  // (or be handled by the caller) ...
  def isEndOfPath(path: Seq[String]): Boolean =
    path.isEmpty || (path == Seq("index.html")) || (path == Seq("index"))

  private def endOfPath(acc: Store.Path):zio.UIO[Option[Store.Path]] =
    ZIO.succeed(if (acc.isEmpty) None else Some(acc.reverse))
}
