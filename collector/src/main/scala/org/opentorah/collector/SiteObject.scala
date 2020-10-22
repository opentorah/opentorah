package org.opentorah.collector

import org.opentorah.store.Path
import org.opentorah.tei.{Body, Tei}
import org.opentorah.util.Files
import scala.xml.{Elem, Node}

abstract class SiteObject(val site: Site) {

  val htmlFile: SiteFile = new SiteFile {
    override protected def siteObject: SiteObject = SiteObject.this

    override def viewer: Viewer = siteObject.viewer

    override def url: Seq[String] = htmlUrl

    override protected def navigationLinks: Seq[NavigationLink] = SiteObject.this.navigationLinks

    override protected def contentElement: Elem = {
      val result = tei
      val withSummary = result.copy(text = result.text.copy(body = new Body.Value(headerSummary ++ result.body.xml)))
      Tei.toXmlElement(teiTransformer(withSummary))
    }
  }

  protected def htmlUrl: Seq[String]

  protected def tei: Tei

  protected def headerSummary: Seq[Node] = Seq.empty

  protected def viewer: Viewer

  // TODO generalize/conditionalize addCalendarDesc
  protected def teiTransformer: Tei.Transformer =
    Site.addPublicationStatement compose Site.addSourceDesc compose Tei.addLanguage

  def isWide: Boolean = false

  // TODO some override it, some do not - ?!
  def title: Option[String] = None

  protected def navigationLinks: Seq[NavigationLink] = Seq.empty

  // TODO use - and handle - Option!
  def facsUrl: Seq[String] = null
}

object SiteObject {

  def resolve(site: Site, parts: Seq[String]): Option[SiteFile] = {
    if (parts.isEmpty) Some(new IndexObject(site).htmlFile) else {
      val tail: Seq[String] = parts.tail
      parts.head match {
        case Hierarchy       .directoryName => HierarchyObject .resolve(site, Path.empty, site.store, tail)
        case CollectionObject.directoryName => CollectionObject.resolve(site, tail)
        case EntityObject    .directoryName => EntityObject    .resolve(site, tail)
        case ReportsObject   .directoryName => ReportsObject   .resolve(site, tail)

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
