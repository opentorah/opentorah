package org.opentorah.collector

import org.opentorah.site.HtmlContent
import org.opentorah.store.Path
import org.opentorah.tei.Pb
import org.opentorah.xml.{Caching, ScalaXml}
import zio.ZIO

final class FacsimileFacet(document: Document, collectionFacet: Collection.CollectionFacsimileFacet) extends
  Facet(document, collectionFacet),
  HtmlContent.FacsimileViewer[Collector]:

  override protected def wrapperCssClass: String = "facsimileWrapper"

  override protected def innerContent(path: Path, collector: Collector): Caching.Parser[ScalaXml.Nodes] =
    val collectionPath: Path = collector.collectionPath(path)
    val facsimileUrl: String =
      val pathStr: String = Path.structureNames(collectionPath).mkString("/")
      collector.common.getTei.facsimilesUrl.getOrElse("/") + pathStr  + "/"

    ZIO.succeed(
      <div class={HtmlContent.facsimileViewer}>
        <div class="facsimileScroller">{
          // TODO generate lists of images and check for missing ones and orphans
          for page: Page <- document.pages(collection.pageType).filterNot(_.pb.isMissing) yield
            val n: String = page.pb.n
            val pageId: String = Pb.pageId(n)
            document.textFacetLink(collectionPath, collector).setFragment(pageId)(
              <figure>
                <img
                id={pageId}
                alt={s"facsimile for page $n"}
                src={page.pb.facs.getOrElse(s"$facsimileUrl$n.jpg")}
                />
                <figcaption>{n}</figcaption>
              </figure>
            )
          }</div>
      </div>
    )

  override protected def moreNavigationLinks(
    collectionPath: Path,
    collector: Collector
  ): Caching.Parser[Seq[ScalaXml.Element]] =
    ZIO.succeed(Seq(document.textFacetLink(collectionPath, collector)(text = "A")))
