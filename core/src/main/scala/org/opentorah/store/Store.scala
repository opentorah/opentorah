package org.opentorah.store

import org.opentorah.metadata.Named
import org.opentorah.xml.{A, Caching, ScalaXml}
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

  def htmlHeadTitle: Option[String] = None

  def navigationLinks(path: Path, context: Context): Caching.Parser[Seq[ScalaXml.Element]] = ZIO.succeed(Seq.empty)

  def wrapperCssClass: String = null

  def header(path: Path, context: Context): Caching.Parser[Option[ScalaXml.Element]] = ZIO.none

  def htmlBodyTitle: Option[ScalaXml.Nodes] = None

  def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] =
    throw IllegalAccessException(s"Called unimplemented Store.content($path, $context)  on $this")
  
  def style: String = "main"

  def viewer: String = Viewer.default

  final def a(path: Path, pathShortener: Path.Shortener): A = Path.a(path, pathShortener)
