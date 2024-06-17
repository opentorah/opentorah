package org.opentorah.collector

import org.opentorah.html.A
import org.opentorah.metadata.Names
import org.opentorah.site.Site
import org.opentorah.store.{Context, Path, Terminal}
import org.opentorah.tei.Tei
import org.opentorah.xml.{Element, Elements, Nodes, Parser}
import zio.ZIO

abstract class Facet(val document: Document, val collectionFacet: CollectionFacet) extends Terminal:
  // TODO titles: .orElse(document.tei.titleStmt.titles.headOption.map(_.xml))

  final override def names: Names = Names(document.name)
  final def collection: Collection = collectionFacet.collection
  final def getTei: Parser[Tei] = collectionFacet.getTei(document)

  override def htmlHeadTitle: Option[String] = None
  override def htmlBodyTitle: Option[Nodes] = None

  final override def header(path: Path, context: Context): Parser[Option[Element]] =
    val collectionPath: Path = Collector.collectionPath(path)
    for
      pathHeader: Elements <- collection.pathHeaderVertical(context, collectionPath)
      header: Element <- collection.documentHeader(document)
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
  ): Parser[Elements] = for
    siblings: (Option[Document], Option[Document]) <- collection.siblings(document)
    (prev: Option[Document], next: Option[Document]) = siblings
    collectionPath: Path = Collector.collectionPath(path)
    prevSeq: Elements <- ZIO.foreach(prev.toSeq)((prev: Document) =>
      for a: A <- prev.facetLink(context, collectionPath, collectionFacet) yield a(Site.Navigation.prev)
    )
    nextSeq: Elements <- ZIO.foreach(prev.toSeq)((next: Document) =>
      for a: A <- next.facetLink(context, collectionPath, collectionFacet) yield a(Site.Navigation.next)
    )
    upA: A <- context.a(collectionPath)
    upSeq: Elements = Seq(upA(Site.Navigation.up))
    moreLinks: Elements <- moreNavigationLinks(collectionPath, context)
  yield
    prevSeq ++
    upSeq ++
    nextSeq ++
    moreLinks

  protected def moreNavigationLinks(
    path: Path,
    context: Context
  ): Parser[Elements]
