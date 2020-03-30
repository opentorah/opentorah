package org.opentorah.store

import java.net.URL
import org.opentorah.entity.{EntitiesList, Entity, EntityReference}
import org.opentorah.metadata.Names
import org.opentorah.xml.Parser

// TODO allow for inline parsing of Entities with Parsable.allMustBe() and from-file parsing with Parsable.parse(from);
// require xml:id on Entities when parsing from file.
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

  override val by: Option[By[EntityHolder]] =
    Some(new Entities.EntitiesBy(selectors, baseUrl, element.by))

  val lists: Seq[EntitiesList] = element.lists.map(_.take(by.get.stores.map(_.entity)))

  override def references: Seq[EntityReference] = Seq.empty

  def findByRef(ref: String): Option[Entity] = by.get.stores.find(_.entity.id.get == ref).map(_.entity)
}

object Entities {

  final class EntitiesBy(
    inheritedSelectors: Seq[Selector],
    baseUrl: URL,
    element: ByElement
  ) extends By.FromElement[EntityHolder](inheritedSelectors, baseUrl, element) {

    override protected def loadFromDirectory(
      fileNames: Seq[String],
      fileInDirectory: String => URL
    ): Seq[Parser[EntityHolder]] = for (fileName <- fileNames) yield {
      val fromUrl: URL = fileInDirectory(fileName)
      Entity.parseWithId(fromUrl, id = fileName)
        .map(entity => new EntityHolder(inheritedSelectors, fromUrl, entity))
    }
  }
}
