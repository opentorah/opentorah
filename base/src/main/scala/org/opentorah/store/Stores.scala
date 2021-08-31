package org.opentorah.store

import org.opentorah.util.Files
import org.opentorah.xml.Parser
import zio.ZIO

trait Stores {
  def findByName(name: String): Caching.Parser[Option[Store]]

  final def findByNameInStores(name: String):  Parser[Option[Store]] =
    findByNameInStores(name, stores)

  def stores: Seq[Store]

  final def findByName[M](
    fullName: String,
    findByName: String => Caching.Parser[Option[M]],
    allowedExtension: String,
    assumeAllowedExtension: Boolean
  ): Caching.Parser[Option[M]] = {
    val (fileName: String, extension: Option[String]) = Files.nameAndExtension(fullName)

    val name: Option[String] =
      if (extension.isDefined && !extension.contains(allowedExtension))
        if (assumeAllowedExtension) Some(fullName) else None
      else
        Some(fileName)

    name.fold[Caching.Parser[Option[M]]](ZIO.none)(findByName)
  }

  // Note: Site calls this twice, and that is why it is not unfolded right here;
  // it's time to start distinguishing terminal stores from non-terminal ones (containers) -
  // or, rather, FindByNames, not Stores;
  // see also acceptsIndexHtml below.
  // Then maybe more rigidity can be introduced and the double call removed...
  final def findByNameInStores(name: String, stores: Seq[Store]): Parser[Option[Store]] =
    ZIO.succeed(stores.find(_.names.hasName(name)))

  /*
    Successful `resolve()` returns a 'path' - a sequence of `Stores`;
    URL resolved by such a path can be reconstructed from it (in its canonical form);
    such a reconstructed URL should resolve to the same path (TODO add tests for this ;)).

    Not all `Stores` are read from XML - some are constructed -
    so `Store` does *not* extend `FromUrl.With`.
  */
  def resolve(
    path: Seq[String],
    acc: Store.Path
  ): Caching.Parser[Option[Store.Path]] = {
    if (isEndOfPath(path)) endOfPath(acc) else for {
      nextOption <- findByName(path.head)
      result <- nextOption match {
        case Some(next: Stores) => next.resolve(path.tail, next +: acc)
        case Some(next) if isEndOfPath(path.tail) => endOfPath(next +: acc)
        case _ => ZIO.succeed(None) // Note: with the None pattern, I get "match may not be exhaustive" warning...
      }
    } yield result
  }

  // TODO this is too HTML-ly for the 'store' package and should be something like isDirectory/isTerminal
  // (or be handled by the caller) ...
  private def isEndOfPath(path: Seq[String]): Boolean =
    path.isEmpty || (path == Seq("index.html")) || (path == Seq("index"))

  private def endOfPath(acc: Store.Path):zio.UIO[Option[Store.Path]] =
    ZIO.succeed(if (acc.isEmpty) None else Some(acc.reverse))
}
