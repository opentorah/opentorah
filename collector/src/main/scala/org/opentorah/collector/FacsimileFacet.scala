package org.opentorah.collector

import org.opentorah.site.Site
import org.opentorah.store.{Context, Path}
import org.opentorah.tei.Pb
import org.opentorah.xml.{A, Caching, ScalaXml}
import zio.ZIO

final class FacsimileFacet(document: Document, collectionFacet: CollectionFacet) extends
  Facet(document, collectionFacet):
  
  override def content(path: Path, context: Context): Caching.Parser[ScalaXml.Element] =
    val collectionPath: Path = Collector.collectionPath(path)
    val facsimileUrl: String =
      val pathStr: String = Path.structureNames(collectionPath).mkString("/")
      Collector.get(context).common.getTei.facsimilesUrl.getOrElse("/") + pathStr  + "/"

    for
      // TODO generate lists of images and check for missing ones and orphans
      pageLinks: Seq[ScalaXml.Element] <- ZIO.foreach(document.pages(collection.pageType).filterNot(_.pb.isMissing))((page: Page) =>
        val n: String = page.pb.n
        val pageId: String = Pb.pageId(n)
        for textFacetA: A <- document.textFacetLink(context, collectionPath) yield
          textFacetA.setFragment(pageId)(
            <figure>
              <img
              id={pageId}
              alt={s"facsimile for page $n"}
              src={page.pb.facs.getOrElse(s"$facsimileUrl$n.jpg")}/>
              <figcaption>
                {n}
              </figcaption>
            </figure>
          )
      )
    yield
      <div class={context.viewer(this)}>
        <div class="facsimileScroller">{pageLinks}</div>
      </div>

  override protected def moreNavigationLinks(
    collectionPath: Path,
    context: Context
  ): Caching.Parser[Seq[ScalaXml.Element]] =
    for
      textFacetA: A <- document.textFacetLink(context, collectionPath)
    yield Seq(textFacetA(text = Site.Navigation.text))
