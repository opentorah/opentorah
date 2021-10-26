package org.opentorah.collector

import org.opentorah.store.{Context, Path, Viewer}
import org.opentorah.tei.Tei
import org.opentorah.xml.{Caching, ScalaXml}

final class TextFacet(document: Document, collectionFacet: CollectionFacet) extends
  Facet(document, collectionFacet),
  Viewer.Text derives CanEqual:

  override def equals(other: Any): Boolean =
    val that: TextFacet = other.asInstanceOf[TextFacet]
    (this.collection == that.collection) && (this.document == that.document)

  override def wrapperCssClass: String = "textWrapper"

  override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] = for
    tei: Tei <- getTei
  yield
    <div>
      {tei.text.body.content.scalaXml}
    </div>

  override protected def moreNavigationLinks(
    collectionPath: Path,
    context: Context
  ): Caching.Parser[Seq[ScalaXml.Element]] = for
    translations <- collection.translations(document)
    pathShortener: Path.Shortener <- context.pathShortener
  yield
    Seq(document.facetLink(collectionPath, collection.facsimileFacet, pathShortener)(text = Tei.facsimileSymbol)) ++
    (for translation <- if document.isTranslation then Seq.empty else translations
     yield translation.textFacetLink(collectionPath, pathShortener)(s"[${translation.lang}]"))
