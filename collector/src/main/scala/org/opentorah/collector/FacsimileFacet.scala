package org.opentorah.collector

import org.opentorah.site.Site
import org.opentorah.store.{Context, Path, Viewer}
import org.opentorah.tei.Pb
import org.opentorah.xml.{Caching, ScalaXml}
import zio.ZIO

final class FacsimileFacet(document: Document, collectionFacet: CollectionFacet) extends
  Facet(document, collectionFacet),
  Viewer.Facsimile:

  override def wrapperCssClass: String = "facsimileWrapper"

  override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] =
    val collectionPath: Path = Collector.collectionPath(path)
    val facsimileUrl: String =
      val pathStr: String = Path.structureNames(collectionPath).mkString("/")
      Collector.get(context).common.getTei.facsimilesUrl.getOrElse("/") + pathStr  + "/"

    for pathShortener: Path.Shortener <- context.pathShortener yield
      <div class={viewer}>
        <div class="facsimileScroller">{
          // TODO generate lists of images and check for missing ones and orphans
          for page: Page <- document.pages(collection.pageType).filterNot(_.pb.isMissing) yield
            val n: String = page.pb.n
            val pageId: String = Pb.pageId(n)
            document.textFacetLink(collectionPath, pathShortener).setFragment(pageId)(
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

  override protected def moreNavigationLinks(
    collectionPath: Path,
    context: Context
  ): Caching.Parser[Seq[ScalaXml.Element]] =
    for pathShortener: Path.Shortener <- context.pathShortener
    yield Seq(document.textFacetLink(collectionPath, pathShortener)(text = Site.Navigation.text))
