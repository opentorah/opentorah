package org.opentorah.collector

import org.opentorah.store.Path
import org.opentorah.tei.{EntityName, Pb, Ref, Tei, Tei2Html}
import org.opentorah.util.Files
import org.opentorah.xml.Xml
import scala.xml.Elem

abstract class SiteObject(val site: Site) {

  final val teiFile: SiteFile = new SiteFile {
    override def url: Seq[String] = teiUrl

    override def content: String = {
      val resolver = site.resolver(facsUrl)
      val elem: Elem = Xml.transform(
        xml = Tei.toXmlElement(teiTransformer(tei)),
        transformer = Ref.transformer(resolver) compose EntityName.transformer(resolver) compose Pb.transformer(resolver)
      )
      Tei.prettyPrinter.renderXml(elem)
    }
  }

  protected def teiTransformer: Tei.Transformer =
    Site.addPublicationStatement compose Site.addSourceDesc compose Tei.addLanguage

  protected def facsUrl: Seq[String] = null

  final val teiWrapperFile: SiteFile = new HtmlFile {
    override def viewer: Viewer = SiteObject.this.viewer

    override def url: Seq[String] = teiWrapperUrl

    override protected def siteParameters: SiteParameters = site.siteParameters

    override protected def contentElement: Elem =
      <script type='module'>import loadTei from '/js/tei.js'; loadTei('{Files.mkUrl(teiFile.url)}');</script>

    override protected def pageParameters: PageParameters = new PageParameters(
      target = Some(viewer),
      style = if (isWide) "wide" else "main",
      title = SiteObject.this.title,
      navigationLinks = SiteObject.this.navigationLinks
    )
  }

  // TODO eventually, teiFile and teiWrapperFile should go away...
  val htmlFile: SiteFile = new HtmlFile {
    override def viewer: Viewer = SiteObject.this.viewer

    override def url: Seq[String] = htmlUrl

    override protected def siteParameters: SiteParameters = site.siteParameters

    override protected def contentElement: Elem = {
      // TODO move into the HtmlFile
      Tei2Html.transform(site.resolver(facsUrl), Tei.toXmlElement(teiTransformer(tei)))
    }

    override protected def pageParameters: PageParameters = new PageParameters(
      target = Some(viewer),
      style = if (isWide) "wide-ng" else "main-ng",
      title = SiteObject.this.title,
      navigationLinks = SiteObject.this.navigationLinks
    )
  }

  // TODO remove
  protected def teiUrl: Seq[String]

  // TODO remove
  protected def teiWrapperUrl: Seq[String]

  protected def htmlUrl: Seq[String] = ??? // TODO

  protected def tei: Tei

  protected def viewer: Viewer

  protected def isWide: Boolean = false

  // TODO some override it, some do not - ?!
  protected def title: Option[String] = None

  protected def navigationLinks: Seq[NavigationLink] = Seq.empty
}

object SiteObject {

  def resolve(site: Site, parts: Seq[String]): Option[SiteFile] = {
    if (parts.isEmpty) Some(new IndexObject(site).teiWrapperFile) else {
      val tail: Seq[String] = parts.tail
      parts.head match {
        case Hierarchy       .directoryName => HierarchyObject .resolve(site, Path.empty, site.store, tail)
        case CollectionObject.directoryName => CollectionObject.resolve(site, tail)
        case EntityObject    .directoryName => EntityObject    .resolve(site, tail)
        case ReportObject    .directoryName => ReportObject    .resolve(site, tail)

        case file if parts.tail.isEmpty =>
          val (fileName: String, extension: Option[String]) = Files.nameAndExtension(file)
          val result: Option[SimpleSiteObject] = fileName match {
            case IndexObject    .fileName => Some(new IndexObject    (site))
            case TreeIndexObject.fileName => Some(new TreeIndexObject(site))
            case NamesObject    .fileName => Some(new NamesObject    (site))
            case _ => None
          }
          result.flatMap(k => SimpleSiteObject.resolve(extension, k))
            // Assume that this is a collection reference:
            .orElse(CollectionObject.resolve(site, parts))

        // Assume that this is a collection reference:
        case _ => CollectionObject.resolve(site, parts)
      }
    }
  }
}
