package org.opentorah.collectorng

import org.opentorah.xml.{Antiparser, Attribute, Element, FromUrl, Parser, Xml}

final class ByHierarchy(
  override val fromUrl: FromUrl,
  override val selector: Selector,
  val stores: Seq[Store]
) extends By with FromUrl.With {

  // TODO move into Site
  def collections(prefix: Store.Path): Seq[Store.Path] = stores.flatMap {
    case collection: Collection => Seq(prefix ++ Seq(this, collection))
    case store: Hierarchy => store.by.toSeq.flatMap(_.collections(prefix ++ Seq(this, store)))
    case _ => Seq.empty
  }

  override def findByName(name: String): Option[Store] = Store.findByName(name, stores)
}

object ByHierarchy extends Element[ByHierarchy]("by") {

  override def parser: Parser[ByHierarchy] = for {
    fromUrl <- currentFromUrl
    selector <- By.selector
    stores <- storeParsable.followRedirects.all
  } yield new ByHierarchy(
    fromUrl,
    selector,
    stores
  )

  override def antiparser: Antiparser[ByHierarchy] = Antiparser.concat(
    By.selectorToXml,
    storeParsable.toXmlSeq(_.stores)
  )

  // TODO I am using existing store XML files, so I re-use 'type' attribute to determine that this is a collection - for now.
  // Once I migrate to the new generation Collector code, the XML files will change,
  // element names for hierarchy and collection store will be different,
  // and I won't have to override Element's toXmlElement here (this will be Element.Union instead).
  // At that point, aniparser override should be removed
  // and Element.toXmlElement made final.
  private val typeAttribute: Attribute[String] = Attribute("type") // "org.opentorah.collector.Book"

  private val storeParsable: Element[Store] = new Element[Store]("store") {
    override def parser: Parser[Store] = for {
      typeOpt <- typeAttribute.optional
      result <- if (typeOpt.isEmpty) Hierarchy.parser else Collection.parser
    } yield result

    override def toXmlElement(value: Store): Xml.Element = value match {
      case value: Hierarchy => Hierarchy.toXmlElement(value)
      case value: Collection     => Collection    .toXmlElement(value)
    }

    override def antiparser: Antiparser[Store] = ???
  }
}
