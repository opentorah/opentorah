package org.opentorah.store

import zio.ZIO

trait FindByName {
  // TODO this is too HTML-ly for the 'store' package and should be something like isDirectory/isTerminal...
  def acceptsIndexHtml: Boolean = false

  def findByName(name: String): Caching.Parser[Option[Store]] = ZIO.none

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
    val theEnd: Boolean = path.isEmpty ||
    // TODO this is to HTML-ly for the 'store' package and should be handled by the caller
      acceptsIndexHtml && ((path == Seq("index.html")) || (path == Seq("index")))

    if (theEnd) ZIO.succeed(if (acc.isEmpty) None else Some(acc.reverse))
    else findByName(path.head) >>=
      (_.fold[Caching.Parser[Option[Store.Path]]](ZIO.none)(next => next.resolve(path.tail, next +: acc)))
  }
}
