package org.opentorah.store

import java.net.URL
import org.opentorah.entity.{EntitiesList, Entity, EntityReference}
import org.opentorah.metadata.{Name, Names}
import org.opentorah.xml.Parser
import scala.xml.Elem

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

  override def toXml: Elem =
    throw new IllegalArgumentException("Not implemented since fromXml is never defined")
}

object Entities {

  final class EntityStore(
    inheritedSelectors: Seq[Selector],
    fromUrl: URL,
    val entity: Entity
  ) extends Store(inheritedSelectors, Some(fromUrl), fromUrl) {

    override def names: Names = new Names(Seq(Name(entity.name)))

    override def references: Seq[EntityReference] = entity.references

    override def toXml: Elem = Entity.toXml(entity.copy(id = None))
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
