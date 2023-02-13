package org.opentorah.collector

import org.opentorah.site.TeiToHtml
import org.opentorah.store.{Context, Path}
import org.opentorah.tei.Tei
import org.opentorah.xml.{A, Caching, ScalaXml}
import zio.ZIO

final class TextFacet(document: Document, collectionFacet: CollectionFacet) extends
  Facet(document, collectionFacet) derives CanEqual:

  override def equals(other: Any): Boolean =
    val that: TextFacet = other.asInstanceOf[TextFacet]
    (this.collection == that.collection) && (this.document == that.document)
  
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
    translations: Seq[Document] <- collection.translations(document)
    translationsToLink: Seq[Document] = if document.isTranslation then Seq.empty else translations
    translationLinks: Seq[ScalaXml.Element] <- ZIO.foreach(translationsToLink)((translation: Document) =>
      for textFacetA: A <- translation.textFacetLink(context, collectionPath)
      yield textFacetA(s"[${translation.lang}]")
    )
    facsimileA: A <- document.facetLink(context, collectionPath, collection.facsimileFacet)
  yield
    Seq(facsimileA(text = TeiToHtml.facsimileSymbol)) ++ translationLinks
