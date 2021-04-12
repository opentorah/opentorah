package org.opentorah.collector

import org.opentorah.metadata.{Language, WithNames}
import org.opentorah.site.Caching
import org.opentorah.util.Files
import org.opentorah.xml.{Elements, Parser, PrettyPrinter, Xml}
import zio.ZIO

// TODO move into the site (or base?) module
trait Store extends WithNames {
  def acceptsIndexHtml: Boolean = false

  def findByName(name: String): Caching.Parser[Option[Store]] = ZIO.none

  def displayName: String = names.doFind(Language.Russian.toSpec).name

  def structureName: String = names.doFind(Language.English.toSpec).name
}

object Store {

  type Path = Seq[Store]

  /*
    Successful `resolve()` returns a 'path' - a sequence of `Stores`;
    URL resolved by such a path can be reconstructed from it (in its canonical form);
    such a reconstructed URL should resolve to the same path (TODO add tests for this ;)).

    Not all `Stores` are read from XML - some are constructed -
     so `Store` does *not* extend `FromUrl.With`.
   */
  def resolve(
    path: Seq[String],
    store: Store,
    acc: Path
  ): Caching.Parser[Option[Path]] = {
    val theEnd: Boolean = path.isEmpty ||
      store.acceptsIndexHtml && ((path == Seq("index.html")) || (path == Seq("index")))

    if (theEnd) ZIO.succeed(if (acc.isEmpty) None else Some(acc.reverse))
    else store.findByName(path.head) >>=
      (_.fold[Caching.Parser[Option[Path]]](ZIO.none)(next => resolve(path.tail, next, next +: acc)))
  }

  def findByName[M](
    fullName: String,
    allowedExtension: String,
    findByName: String => Caching.Parser[Option[M]],
    assumeAllowedExtension: Boolean = false
  ): Caching.Parser[Option[M]] = {
    val (fileName: String, extension: Option[String]) = Files.nameAndExtension(fullName)

    val name: Option[String] =
      if (extension.isDefined && !extension.contains(allowedExtension))
        if (assumeAllowedExtension) Some(fullName) else None
      else
        Some(fileName)

    name.fold[Caching.Parser[Option[M]]](ZIO.none)(name => findByName(name))
  }

  def findByName(name: String, stores: Seq[Store]): Parser[Option[Store]] =
    ZIO.succeed(stores.find(_.names.hasName(name)))

  def renderXml[T <: Store](elements: Elements[T], value: T): String = renderXml(elements.xmlElement(value))
  def renderXml(xml: Xml.Element): String = prettyPrinter.renderXml(xml)

  private val prettyPrinter: PrettyPrinter = new PrettyPrinter(
    nestElements = Set("p"),
    alwaysStackElements = Set("store", "by")
  )
}
