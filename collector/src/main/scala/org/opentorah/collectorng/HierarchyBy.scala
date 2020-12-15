package org.opentorah.collectorng

import org.opentorah.xml.{Antiparser, Attribute, Element, FromUrl, Parser, ToXml, Xml}

// TODO ByHierarchy
final class HierarchyBy(
  override val fromUrl: FromUrl,
  override val selector: Selector,
  val stores: Seq[Store]
) extends By {
  override protected def resolveStore(url: Seq[String]): Option[SiteFile] = None // TODO
}

object HierarchyBy extends Element[HierarchyBy]("by") {
  private val selectorAttribute: Attribute[String] = Attribute("selector")

  override def parser: Parser[HierarchyBy] = for {
    fromUrl <- currentFromUrl
    selector <- selectorAttribute.required
    stores <- storeParsable.followRedirects.all
  } yield new HierarchyBy(
    fromUrl,
    Selector.byName(selector),
    stores
  )

  override def antiparser: Antiparser[HierarchyBy] = Antiparser.concat(
    selectorAttribute.toXml.compose(_.selector.name),
    storeParsable.toXmlSeq.compose(_.stores)
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
      result <- if (typeOpt.isEmpty) HierarchyStore.parser else Collection.parser
    } yield result

    override def toXmlElement(value: Store): Xml.Element = value match {
      case value: HierarchyStore => HierarchyStore.toXmlElement(value)
      case value: Collection     => Collection    .toXmlElement(value)
    }

    override def antiparser: Antiparser[Store] = ???
  }
}
