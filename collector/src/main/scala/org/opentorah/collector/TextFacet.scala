package org.opentorah.collector

import org.opentorah.site.HtmlContent
import org.opentorah.store.Path
import org.opentorah.tei.Tei
import org.opentorah.xml.{Caching, ScalaXml}

final class TextFacet(document: Document, collectionFacet: Collection.CollectionTextFacet) extends
  Facet(document, collectionFacet),
  HtmlContent.TextViewer[Collector] derives CanEqual:

  override def equals(other: Any): Boolean =
    val that: TextFacet = other.asInstanceOf[TextFacet]
    (this.collection == that.collection) && (this.document == that.document)

  override protected def wrapperCssClass: String = "textWrapper"

  override protected def innerContent(path: Path, collector: Collector): Caching.Parser[ScalaXml.Nodes] = for
    tei: Tei <- getTei
  yield
    tei.text.body.content.scalaXml

  override protected def moreNavigationLinks(
    collectionPath: Path,
    collector: Collector
  ): Caching.Parser[Seq[ScalaXml.Element]] = for
    translations <- collection.translations(document)
  yield
    Seq(document.facetLink(collectionPath, collection.facsimileFacet, collector)(text = Tei.facsimileSymbol)) ++
    (for translation <- if document.isTranslation then Seq.empty else translations
     yield translation.textFacetLink(collectionPath, collector)(s"[${translation.lang}]"))
