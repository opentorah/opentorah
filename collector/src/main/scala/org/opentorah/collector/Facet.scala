package org.opentorah.collector

import org.opentorah.metadata.Names
import org.opentorah.site.HtmlContent
import org.opentorah.store.{Path, Terminal}
import org.opentorah.tei.Tei
import org.opentorah.xml.{Caching, ScalaXml}

abstract class Facet(val document: Document, val collectionFacet: Collection.CollectionFacet[?]) extends
  Terminal,
  HtmlContent[Collector]:

  // TODO titles: .orElse(document.tei.titleStmt.titles.headOption.map(_.xml))

  final override def names: Names = Names(document.name)
  final def collection: Collection = collectionFacet.collection
  final def getTei: Caching.Parser[Tei] = collectionFacet.getTei(document)

  override def htmlHeadTitle: Option[String] = None

  final override def content(path: Path, collector: Collector): Caching.Parser[ScalaXml.Element] =
    val collectionPath: Path = collector.collectionPath(path)
    for
      header <- collection.documentHeader(document)
      innerContent <- innerContent(path, collector)
    yield
      <div class={wrapperCssClass}>
        <div class="store-header">
          {collection.pathHeaderVertical(collectionPath, collector)}
          <l>
            {Hierarchical.displayName(collectionFacet)}
            {Hierarchical.displayName(this)}
          </l>
          {header}
        </div>
        {innerContent}
      </div>

  protected def wrapperCssClass: String

  protected def innerContent(path: Path, collector: Collector): Caching.Parser[ScalaXml.Nodes]

  final override def navigationLinks(
    path: Path,
    collector: Collector
  ): Caching.Parser[Seq[ScalaXml.Element]] = for
    siblings: (Option[Document], Option[Document]) <- collection.siblings(document)
    collectionPath: Path = collector.collectionPath(path)
    moreLinks: Seq[ScalaXml.Element] <- moreNavigationLinks(collectionPath, collector)
  yield
    val (prev: Option[Document], next: Option[Document]) = siblings

    prev.toSeq.map(_.facetLink(collectionPath, collectionFacet, collector)("⇦")) ++
    Seq(             collector.a(collectionPath)                          ("⇧")) ++
    next.toSeq.map(_.facetLink(collectionPath, collectionFacet, collector)("⇨")) ++
    moreLinks

  protected def moreNavigationLinks(
    path: Path,
    collector: Collector
  ): Caching.Parser[Seq[ScalaXml.Element]]
