package org.opentorah.store

import java.net.URL
import org.opentorah.entity.{EntitiesList, Entity, EntityReference}
import org.opentorah.metadata.{Name, Names}
import org.opentorah.xml.Parser

// TODO this should have two Bys.
final class Entities(
  inheritedSelectors: Seq[Selector],
  baseUrl: URL,
  element: EntitiesElement
) extends Store(
  inheritedSelectors,
  fromUrl = None,
  baseUrl
) {
  def selector: Selector = selectorByName(element.selector)

  override def names: Names = selector.names

  override val by: Option[By[Entities.EntityStore]] =
    Some(new Entities.EntitiesBy(selectors, baseUrl, element.by))

  val lists: Seq[EntitiesList] = element.lists.map(_.take(by.get.stores.map(_.entity)))

  override def references: Seq[EntityReference] = Seq.empty

  def findByRef(ref: String): Option[Entity] = by.get.stores.find(_.entity.id.get == ref).map(_.entity)
}

object Entities {

  final class EntityStore(
    inheritedSelectors: Seq[Selector],
    fromUrl: URL,
    val entity: Entity
  ) extends Store(inheritedSelectors, Some(fromUrl), fromUrl) {

    override def names: Names = new Names(Seq(Name(entity.name)))

    override def references: Seq[EntityReference] = entity.references
  }

  final class EntitiesBy(
    inheritedSelectors: Seq[Selector],
    baseUrl: URL,
    element: ByElement
  ) extends By[EntityStore](inheritedSelectors, baseUrl, element) {

    override protected def loadFromDirectory(
      fileNames: Seq[String],
      fileInDirectory: String => URL
    ): Seq[Parser[EntityStore]] = for (fileName <- fileNames) yield {
      val fromUrl: URL = fileInDirectory(fileName)
      Entity.parseWithId(fromUrl, id = fileName)
        .map(entity => new EntityStore(inheritedSelectors, fromUrl, entity))
    }
  }
}
