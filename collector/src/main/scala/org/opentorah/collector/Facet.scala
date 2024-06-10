package org.opentorah.collector

import org.opentorah.html.A
import org.opentorah.metadata.Names
import org.opentorah.site.Site
import org.opentorah.store.{Context, Path, Terminal}
import org.opentorah.tei.Tei
import org.opentorah.xml.{Caching, Xml}
import zio.ZIO

abstract class Facet(val document: Document, val collectionFacet: CollectionFacet) extends Terminal:
  // TODO titles: .orElse(document.tei.titleStmt.titles.headOption.map(_.xml))

  final override def names: Names = Names(document.name)
  final def collection: Collection = collectionFacet.collection
  final def getTei: Caching.Parser[Tei] = collectionFacet.getTei(document)

  override def htmlHeadTitle: Option[String] = None
  override def htmlBodyTitle: Option[Xml.Nodes] = None

  final override def header(path: Path, context: Context): Caching.Parser[Option[Xml.Element]] =
    val collectionPath: Path = Collector.collectionPath(path)
    for
      pathHeader: Seq[Xml.Element] <- collection.pathHeaderVertical(context, collectionPath)
      header: Xml.Element <- collection.documentHeader(document)
    yield Some(
      <div class="store-header">
        {pathHeader}
        <l>
          {Hierarchical.displayName(collectionFacet)}
          {Hierarchical.displayName(this)}
        </l>
        {header}
      </div>
    )

  final override def navigationLinks(
    path: Path,
    context: Context
  ): Caching.Parser[Seq[Xml.Element]] = for
    siblings: (Option[Document], Option[Document]) <- collection.siblings(document)
    (prev: Option[Document], next: Option[Document]) = siblings
    collectionPath: Path = Collector.collectionPath(path)
    prevSeq: Seq[Xml.Element] <- ZIO.foreach(prev.toSeq)((prev: Document) =>
      for a: A <- prev.facetLink(context, collectionPath, collectionFacet) yield a(Site.Navigation.prev)
    )
    nextSeq: Seq[Xml.Element] <- ZIO.foreach(prev.toSeq)((next: Document) =>
      for a: A <- next.facetLink(context, collectionPath, collectionFacet) yield a(Site.Navigation.next)
    )
    upA: A <- context.a(collectionPath)
    upSeq: Seq[Xml.Element] = Seq(upA(Site.Navigation.up))
    moreLinks: Seq[Xml.Element] <- moreNavigationLinks(collectionPath, context)
  yield
    prevSeq ++
    upSeq ++
    nextSeq ++
    moreLinks

  protected def moreNavigationLinks(
    path: Path,
    context: Context
  ): Caching.Parser[Seq[Xml.Element]]
