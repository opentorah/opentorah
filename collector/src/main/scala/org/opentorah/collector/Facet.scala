package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.site.Site
import org.opentorah.store.{Context, Path, Terminal}
import org.opentorah.tei.Tei
import org.opentorah.xml.{Caching, ScalaXml}

abstract class Facet(val document: Document, val collectionFacet: CollectionFacet) extends Terminal:
  // TODO titles: .orElse(document.tei.titleStmt.titles.headOption.map(_.xml))

  final override def names: Names = Names(document.name)
  final def collection: Collection = collectionFacet.collection
  final def getTei: Caching.Parser[Tei] = collectionFacet.getTei(document)

  override def htmlHeadTitle: Option[String] = None
  override def htmlBodyTitle: Option[ScalaXml.Nodes] = None

  final override def header(path: Path, context: Context): Caching.Parser[Option[ScalaXml.Element]] =
    val collectionPath: Path = Collector.collectionPath(path)
    for
      header <- collection.documentHeader(document)
      pathShortener: Path.Shortener <- context.pathShortener
    yield Some(
      <div class="store-header">
        {collection.pathHeaderVertical(collectionPath, pathShortener)}
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
  ): Caching.Parser[Seq[ScalaXml.Element]] = for
    siblings: (Option[Document], Option[Document]) <- collection.siblings(document)
    collectionPath: Path = Collector.collectionPath(path)
    moreLinks: Seq[ScalaXml.Element] <- moreNavigationLinks(collectionPath, context)
    pathShortener: Path.Shortener <- context.pathShortener
  yield
    val (prev: Option[Document], next: Option[Document]) = siblings

    prev.toSeq.map(_.facetLink(collectionPath, collectionFacet, pathShortener)(Site.Navigation.prev)) ++
    Seq(                     a(collectionPath                 , pathShortener)(Site.Navigation.up  )) ++
    next.toSeq.map(_.facetLink(collectionPath, collectionFacet, pathShortener)(Site.Navigation.next)) ++
    moreLinks

  protected def moreNavigationLinks(
    path: Path,
    context: Context
  ): Caching.Parser[Seq[ScalaXml.Element]]
