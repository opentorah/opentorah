package org.opentorah.collectorng

import org.opentorah.tei.EntitiesList
import org.opentorah.xml.{Antiparser, Attribute, Element, FromUrl, Parser}

// TODO introduce EntitiesBy?
final class Entities(
  override val fromUrl: FromUrl,
  val selector: Selector,
  val bySelector: Selector,
  val directory: String,
  val lists: Seq[EntitiesList]
) extends FromUrl.With {
  // TODO write a list with id (file name), entity type and role;
  // add those to the EntityHolder;
  // make EntitiesList contain and findByRef return EntityHolder;
  // load Entity in the EntityHolder on demand.

//  lazy val lists: Seq[EntitiesList] = element.lists.map(_.take(by.get.stores.map(_.entity)))

  def fileById(id: String): Option[EntityFile] = Some(new EntityFile(id))
}

object Entities extends Element[Entities]("entities") {

  private val selectorPluralAttribute: Attribute[String] = Attribute("selectorPlural")
  private val selectorSingularAttribute: Attribute[String] = Attribute("selectorSingular")
  private val directoryAttribute: Attribute[String] = Attribute("directory")

  override def parser: Parser[Entities] = for {
    fromUrl <- currentFromUrl
    selector <- selectorPluralAttribute.required
    bySelector <- selectorSingularAttribute.required
    directory <- directoryAttribute.required
    lists <- EntitiesList.all
  } yield new Entities(
    fromUrl,
    Selector.byName(selector),
    Selector.byName(bySelector),
    directory,
    lists
  )

  override val antiparser: Antiparser[Entities] = Antiparser.concat(
    selectorPluralAttribute.toXml.compose(_.selector.name),
    selectorSingularAttribute.toXml.compose(_.bySelector.name),
    directoryAttribute.toXml.compose(_.directory),
    EntitiesList.toXmlSeq.compose(_.lists)
  )
}
