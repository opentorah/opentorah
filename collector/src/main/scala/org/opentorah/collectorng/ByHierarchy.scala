package org.opentorah.collectorng

import org.opentorah.xml.{Antiparser, Attribute, Element, FromUrl, Parsable, Parser, Xml}

final class ByHierarchy(
  override val fromUrl: FromUrl,
  override val selector: Selector,
  val stores: Seq[Store]
) extends By with FromUrl.With with HtmlContent {

  // TODO move into Site
  def collections(prefix: Store.Path): Seq[Store.Path] = stores.flatMap {
    case collection: Collection => Seq(prefix ++ Seq(this, collection))
    case store: Hierarchy => store.by.toSeq.flatMap(_.collections(prefix ++ Seq(this, store)))
    case _ => Seq.empty
  }

  override def findByName(name: String): Option[Store] = Store.findByName(name, stores)

  override def viewer: Html.Viewer = Html.Viewer.default
  override def isWide: Boolean = false
  override def htmlTitle: Option[String] = selector.title
  override def navigationLinks: Seq[Html.NavigationLink] = Seq.empty
  override def lang: Option[String] = None
  override def content(site: Site): Xml.Element = ???
}

object ByHierarchy extends Element[ByHierarchy]("by") {

  override def contentParsable: Parsable[ByHierarchy] = new Parsable[ByHierarchy] {
    override def parser: Parser[ByHierarchy] = for {
      fromUrl <- Element.currentFromUrl
      selector <- By.selector
      stores <- storeParsable.followRedirects.seq()
    } yield new ByHierarchy(
      fromUrl,
      selector,
      stores
    )

    override def antiparser: Antiparser[ByHierarchy] = Antiparser.concat(
      By.selectorToXml,
      storeParsable.seq(_.stores)
    )
  }

  // TODO I am using existing store XML files, so I re-use 'type' attribute to determine that this is a collection - for now.
  // Once I migrate to the new generation Collector code, the XML files will change,
  // element names for hierarchy and collection store will be different,
  // and I won't have to override Element's toXmlElement here (this will be Element.Union instead).
  // At that point, aniparser override should be removed
  // and Element.toXmlElement made final.
  private val typeAttribute: Attribute.Optional[String] = Attribute("type").optional // "org.opentorah.collector.Book"

  private val storeParsable: Element[Store] = new Element[Store]("store") {
    override def contentParsable: Parsable[Store] = new Parsable[Store] {
      override def parser: Parser[Store] = for {
        typeOpt <- typeAttribute()
        result <- if (typeOpt.isEmpty) Hierarchy.contentParsable() else Collection.contentParsable()
      } yield result

      override def antiparser: Antiparser[Store] = ???
    }

    override protected def elementByValue(value: Store): Element[_] = value match {
      case value: Hierarchy   => Hierarchy
      case value: Collection  => Collection
    }
  }
}
