package org.opentorah.collector

import org.opentorah.store.Path
import org.opentorah.tei.Tei
import org.opentorah.util.{Files, Xml}
import scala.xml.Elem

abstract class SiteObject(val site: Site) {

  def viewer: String

  def teiFile: SiteFile = new SiteFile {
    override def siteObject: SiteObject = SiteObject.this

    override def url: Seq[String] = teiUrl

    override def content: String = {
      val elem: Elem = Xml.transform(
        xml = Tei.toXml(teiTransformer(tei)),
        transformer = xmlTransformer
      )
      Xml.xmlHeader + Transformations.teiPrettyPrinter.render(elem) +  "\n"
    }
  }

  def teiWrapperFile: SiteFile = new SiteFile {
    override def siteObject: SiteObject = SiteObject.this

    override def url: Seq[String] = teiWrapperUrl

    final def content: String = SiteObject.withYaml(
      yaml = style.fold[Seq[(String, String)]](Seq.empty)(style => Seq("style" -> style)) ++
        Seq(
          "layout" -> "tei",
          "tei" -> Files.mkUrl(siteObject.teiFile.url),
          "target" -> siteObject.viewer
        ) ++ yaml
    )
  }

  protected def teiUrl: Seq[String]

  protected def teiTransformer: Tei.Transformer = Transformations.addCommonNoCalendar

  protected def xmlTransformer: Xml.Transformer = Transformations.refRoleRewriter(site)

  protected def tei: Tei

  protected def teiWrapperUrl: Seq[String]

  protected def style: Option[String] = None

  protected def yaml: Seq[(String, String)] = Seq.empty
}

object SiteObject {

  def resolve(site: Site, parts: Seq[String]): Option[SiteFile] = {
      if (parts.isEmpty) Some(new IndexObject(site).teiWrapperFile) else {
        val tail: Seq[String] = parts.tail
        parts.head match {
          case HierarchyObject.directoryName  => HierarchyObject.resolve(site, Path.empty, site.store, tail)
          case CollectionObject.directoryName => CollectionObject.resolve(site, tail)
          case EntityObject.directoryName     => EntityObject.resolve(site, tail)

          case file if parts.tail.isEmpty =>
            val (fileName: String, extension: Option[String]) = Files.nameAndExtension(file)
            val result: Option[SimpleSiteObject] = fileName match {
              case IndexObject.fileName     => Some(new IndexObject    (site))
              case TreeIndexObject.fileName => Some(new TreeIndexObject(site))
              case NamesObject.fileName     => Some(new NamesObject    (site))
              case _ => None
            }
            result.flatMap(k => SimpleSiteObject.resolve(extension, k))

          case _ => None
        }
      }
    }

  def withYaml(
    yaml: Seq[(String, String)],
    content: Seq[String] = Seq.empty
  ): String = {
    val result: Seq[String] =
      Seq("---") ++
      (for ((name, value) <- yaml) yield name + ": " + quote(value)) ++
      Seq("---") ++
      Seq("") ++ content

    result.mkString("", "\n", if (content.nonEmpty) "\n" else "")
  }

  private def quote(what: String): String = s"'$what'"
}
